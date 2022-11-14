{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ClosureConvert where

import Lang
import IR
import Control.Monad.State
import Control.Monad.Writer
import Subst

type M a = StateT Int (Writer [IrDecl]) a

closureConvert :: TTerm -> M Ir

closureConvert (V _ (Bound i)) = error "mal closureConvert"
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = return $ IrGlobal n
closureConvert (Const _ c) = return $ IrConst c
closureConvert (Lam (_,fty) n _ t@(Sc1 _)) = do fr <-  get
                                                put $ fr + 1
                                                let frName = "__f" ++ show fr
                                                body <- closureConvert (open n t)
                                                
                                                --ird <- IrFun frName (convertType fty) args body1
                                                --tell ird
                                                return $ MkClosure frName []
                                        -- arnar mk clousure
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
                                            return $ IrLet n (convertType ty) decl sc
-- closureConvert (App _ t1 t2) = return $ IrCall (closureConvert t1) (closureConvert t2)   --seria un ircall pero how y pq lista en 2do  
-- app y fix ?

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