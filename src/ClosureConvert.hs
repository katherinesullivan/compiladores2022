{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ClosureConvert where

import Lang
import IR
import Control.Monad.State
import Control.Monad.Writer
import Subst
import           Data.List.Extra            ( nubSortBy )


type M a = StateT Int (Writer [IrDecl]) a

closureConvert :: TTerm -> M Ir

closureConvert (V _ (Bound i)) = error "mal closureConvert"
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = return $ IrGlobal n
closureConvert (Const _ c) = return $ IrConst c
closureConvert (Lam (_,fty) n _ t@(Sc1 _)) = do fr <- get
                                                put $ fr + 1
                                                let frName = "__f" ++ show fr
                                                let openT = open n t
                                                body <- closureConvert openT --
                                                let freeT = freeTTermVars openT
                                                let clo = body 
                                                env <- get
                                                put $ env + 1
                                                let envName = "__env" ++ show env
                                                var <- get
                                                put $ var + 1
                                                let varName = "__var" ++ show var
                                                let args = [(envName, IrClo), (varName, IrInt)]
                                                let ird = IrFun frName (convertType fty) args body
                                                tell [ird]
                                                return $ MkClosure frName (map (IrVar . fst) freeT)
-- 1. armar el irdecl y gurdarlo en la monada
-- 2. devolver el MkClosure
--  name -> fresco
--  args -> las vars libres 
--  body -> llamada rec + cerrar y subst 1 vez ?

closureConvert (BinaryOp _ op t1 t2) = IrBinaryOp op <$> closureConvert t1 <*> closureConvert t2
closureConvert (IfZ _ t1 t2 t3) = IrIfZ <$> closureConvert t1 <*> closureConvert t2 <*> closureConvert t3
closureConvert (Print _ s t1) = IrPrint s <$> closureConvert t1
closureConvert (Let _ n ty t1 (Sc1 t)) = do decl <- closureConvert t1
                                            sc <- closureConvert t 
                                            return $ IrLet n (convertType ty) decl sc -- porque ignorabamos que de t escapa la variable n?
-- closureConvert (App _ t1 t2) = return $ IrCall (closureConvert t1) (closureConvert t2)   --seria un ircall pero how y pq lista en 2do  
-- app y fix ?

freeTTermVars :: TTerm -> [(Name,IrTy)]
freeTTermVars tm = nubSortBy (\x y -> compare (fst x) (fst y)) $ go tm [] where
  go (V (_, ty) (Free   v)    ) xs = (v,convertType ty) : xs
  go (V (_, ty) (Global v)    ) xs = (v,convertType ty) : xs
  go (V _ _                   ) xs = xs
  go (Lam _ _ _ (Sc1 t)       ) xs = go t xs
  go (App   _ l r             ) xs = go l $ go r xs
  go (Print _ _ t             ) xs = go t xs
  go (BinaryOp _ _ t u        ) xs = go t $ go u xs
  go (Fix _ _ _ _ _ (Sc2 t)   ) xs = go t xs
  go (IfZ _ c t e             ) xs = go c $ go t $ go e xs
  go (Const _ _               ) xs = xs
  go (Let _ _ _ e (Sc1 t)     ) xs = go e (go t xs)

convertType :: Ty -> IrTy
convertType NatTy = IrInt
convertType (FunTy _ _) = IrClo

runCC' :: [Decl TTerm] -> M [IrDecl]
runCC' [] = return []
runCC' (d@Decl{}:ds) = do
    tt <- closureConvert (declBody d)
    dst <- runCC' ds
    return $ IrVal (declName d) (convertType $ declType d) tt:dst

runCC :: [Decl TTerm] -> [IrDecl]
runCC mod1 = 
    let ((vals, _stfinal), accs) = runWriter (runStateT (runCC' mod1) 0) in
        accs ++ vals -- ver orden