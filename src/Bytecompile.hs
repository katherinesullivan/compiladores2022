{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Bytecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite compilar módulos a la Macchina. También provee
una implementación de la Macchina para ejecutar el bytecode.
-}

module Bytecompile
  (Bytecode, runBC, bcWrite, bcRead, bytecompileModule, showBC)
 where

import Lang
import Subst
import MonadFD4

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )

import Data.List (intercalate)
import Data.Char

type Opcode = Int
type Bytecode = [Int]
type Environment = [Val]
data Val = I Int | Fun Environment Bytecode | RA Environment Bytecode deriving Show
type Stack = [Val]
data EFix = FixFun EFix Bytecode Environment

newtype Bytecode32 = BC { un32 :: [Word32] }

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go
    where go =
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:

   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern NULL     = 0
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern ADD      = 6
pattern SUB      = 7
pattern FIX      = 9
pattern STOP     = 10
pattern SHIFT    = 11
pattern DROP     = 12
pattern PRINT    = 13
pattern PRINTN   = 14
pattern JUMP     = 15
pattern CJUMP      = 16
pattern TAILCALL = 17

--función util para debugging: muestra el Bytecode de forma más legible.
showOps :: Bytecode -> [String]
showOps [] = []
showOps (NULL:xs)        = "NULL" : showOps xs
showOps (RETURN:xs)      = "RETURN" : showOps xs
showOps (CONST:i:xs)     = ("CONST " ++  show i) : showOps xs
showOps (ACCESS:i:xs)    = ("ACCESS " ++ show i) : showOps xs
showOps (FUNCTION:i:xs)  = ("FUNCTION len=" ++ show i) : showOps xs
showOps (CALL:xs)        = "CALL" : showOps xs
showOps (ADD:xs)         = "ADD" : showOps xs
showOps (SUB:xs)         = "SUB" : showOps xs
showOps (FIX:xs)         = "FIX" : showOps xs
showOps (STOP:xs)        = "STOP" : showOps xs
showOps (JUMP:i:xs)      = ("JUMP off=" ++ show i) : showOps xs
showOps (SHIFT:xs)       = "SHIFT" : showOps xs
showOps (DROP:xs)        = "DROP" : showOps xs
showOps (PRINT:xs)       = let (msg,_:rest) = span (/=NULL) xs
                           in ("PRINT " ++ show (bc2string msg)) : showOps rest
showOps (PRINTN:xs)      = "PRINTN" : showOps xs
showOps (CJUMP:xs)       = "CJUMP" : showOps xs
showOps (x:xs)           = show x : showOps xs

showBC :: Bytecode -> String
showBC = intercalate "; " . showOps

-- Compila un término a bytecode
bc :: MonadFD4 m => TTerm -> m Bytecode
bc tt = do let t = bcc tt
           printFD4(showBC t)
           return t

bcc :: TTerm -> Bytecode
bcc (V _ (Bound i)) = [ACCESS, i]
bcc (V _ (Free n)) = error "No deberia haber una variable free"
bcc (V _ (Global n)) = error "No deberia haber una variable global"
bcc (Const _ (CNat n)) = [CONST, n]
bcc (Lam _ _ _ (Sc1 t)) = let bct = bcc t
                          in [FUNCTION, length bct + 1] ++ bct ++ [RETURN]
bcc (App _ t1 t2) = bcc t1 ++ bcc t2 ++ [CALL]
bcc (Print _ s t) | s == "" = bcc t ++ [PRINTN]
                  | otherwise = [PRINT] ++ string2bc s ++ [NULL] ++ bcc t ++ [PRINTN]
bcc (BinaryOp _ Add x y) = bcc x ++ bcc y ++ [ADD]
bcc (BinaryOp _ Sub x y) = bcc x ++ bcc y ++ [SUB]
bcc (Fix _ _ _ _ _ (Sc2 t)) = let bct = bccT t
                              in [FUNCTION, length bct + 1] ++ bct ++ [RETURN, FIX]
bcc (IfZ _ b t f) = let tbc = bcc t
                        fbc = bcc f
                    in bcc b ++ [CJUMP, length tbc + 2] ++ tbc ++ [JUMP, length fbc] ++ fbc
bcc (Let _ _ _ t' (Sc1 t)) = bcc t' ++ [SHIFT] ++ bcc t ++ [DROP]

bccT :: TTerm -> Bytecode
bccT (App _ t1 t2) = bcc t1 ++ bcc t2 ++ [TAILCALL]
bccT (Let _ _ _ t' (Sc1 t)) = bcc t' ++ [SHIFT] ++ bccT t
bccT t = bcc t ++ [RETURN]


-- ord/chr devuelven los codepoints unicode, o en otras palabras
-- la codificación UTF-32 del caracter.
string2bc :: String -> Bytecode
string2bc = map ord

bc2string :: Bytecode -> String
bc2string = map chr

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule m = let m1 = global2free m
                          m2 = decl2nestedLet m1
                          m3 = bcc m2
                      in return (m3 ++ [STOP])

global2free :: Module -> Module
global2free = map (\ (Decl i n ty term) -> Decl i n ty (rename term))
                where rename (V i (Global n)) =  V i (Free n)
                      rename t@(V _ _) = t
                      rename t@(Const _ _) = t
                      rename (Lam i n ty (Sc1 t)) = Lam i n ty (Sc1 (rename t))
                      rename (App i t1 t2) = App i (rename t1) (rename t2)
                      rename (Print i s t) = Print i s (rename t)
                      rename (BinaryOp i b t1 t2) = BinaryOp i b (rename t1) (rename t2)
                      rename (Fix i x xty f fty (Sc2 t)) = Fix i x xty f fty (Sc2 (rename t))
                      rename (IfZ i t1 t2 t3) = IfZ i (rename t1) (rename t2) (rename t3)
                      rename (Let i x xty t1 (Sc1 t2)) = Let i x xty (rename t1) (Sc1 (rename t2))


decl2nestedLet :: Module -> TTerm 
decl2nestedLet [] = error "empty module"
decl2nestedLet [m1] = declBody m1
decl2nestedLet (m1:m) = Let (declPos m1, declType m1) (declName m1) (declType m1) (declBody m1) (close (declName m1) (decl2nestedLet m))


-- | Toma un bytecode, lo codifica y lo escribe un archivo
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = (map fromIntegral <$> un32) . decode <$> BS.readFile filename

runBC :: MonadFD4 m => Bytecode -> m ()
runBC bcs = runBCWithArgs bcs [] []

runBCWithArgs :: MonadFD4 m => Bytecode -> Environment -> Stack -> m ()
runBCWithArgs (ACCESS:i:bcs) e s = let n = e!!i
                                       s' = n:s
                                   in runBCWithArgs bcs e s'
runBCWithArgs (CONST:n:bcs) e s = let s' = I n:s 
                                  in runBCWithArgs bcs e s' 
runBCWithArgs (ADD:bcs) e (I n:I m:s) = let s' = I (m+n):s
                                        in runBCWithArgs bcs e s' 
runBCWithArgs (SUB:bcs) e (I n:I m:s) = let resta = m-n 
                                            r = if resta <= 0 then 0 else resta
                                            s' = I r:s
                                        in runBCWithArgs bcs e s'
runBCWithArgs (CALL:bcs) e (v:Fun ef cf:s) = let e' = v:ef 
                                                 s' = RA e bcs : s
                                             in runBCWithArgs cf e' s' 
runBCWithArgs (FUNCTION:len:bcs) e s = let cf = take len bcs  -- chequear que take y drop funcionen bien con la longitud -> menaing que no tiren o guarden algo de mas
                                           c = drop len bcs
                                           s' = Fun e cf:s 
                                       in runBCWithArgs c e s'
runBCWithArgs (RETURN:_) _ (v:RA e c:s) = let s' = v:s 
                                          in runBCWithArgs c e s'
runBCWithArgs (PRINTN:bcs) e s@(I n:_) = do printFD4 (show n) 
                                            runBCWithArgs bcs e s
runBCWithArgs (PRINT:bcs) e s = let strInBC = takeWhile (/= NULL) bcs
                                    str = bc2string strInBC
                                    c = dropWhile (/= NULL) bcs 
                                in do printFD4NoNL str
                                      runBCWithArgs c e s
runBCWithArgs (NULL:bcs) e s = runBCWithArgs bcs e s
runBCWithArgs (CJUMP:len:bcs) e (I n:s) = if n == 0 then runBCWithArgs bcs e s else runBCWithArgs (drop len bcs) e s
runBCWithArgs (JUMP:len:bcs) e s  = runBCWithArgs (drop len bcs) e s
runBCWithArgs (STOP:_) _ _ = return ()
runBCWithArgs (SHIFT:bcs) e (v:s) = runBCWithArgs bcs (v:e) s
runBCWithArgs (DROP:bcs) (v:e) s = runBCWithArgs bcs e s
runBCWithArgs (FIX:bcs) e (Fun env cf:s) = let efix = Fun efix cf : env
                                           in runBCWithArgs bcs e (Fun efix cf:s)
runBCWithArgs _ _ _ = failFD4 "Malformed ByteCode"
