module ClosureConvert where

import Lang
import IR
import Control.Monad.State
import Control.Monad.Writer
import Parse (tm)

type M a = StateT Int (Writer [IrDecl]) a

closureConvert :: Term -> M Ir

closureConvert (V _ (Bound i)) = return
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = return $ IrGlobal n
closureConvert (Const _ c) = return $ IrConst c
closureConvert (Lam (_,fty) n _ (Sc1 t)) = do  fr <-  get
                                               put $ fr + 1
                                               frName <- "__f" ++ show fr
                                               body <- closureConvert t
                                               ird <- IrFun frName (convertType fty) args body
                                               tell ird
                                               return $ MkClosure frName args
                                        -- arnar mk clousure
-- 1. armar el irdecl y gurdarlo en la monada
-- 2. devolver el MkClosure
--  name -> fresco
--  args ?
--  body -> llamada rec + cerrar y subst 1 vez ?

closureConvert (BinaryOp _ op t1 t2) = return $ IrBinaryOp op (closureConvert t1) (closureConvert t2)
closureConvert (IfZ _ t1 t2 t3) = return $ IrIfZ (closureConvert t1) (closureConvert t2) (closureConvert t3)
closureConvert (Print _ s t1) = return $ IrPrint s t1
closureConvert (Let _ n ty tm (Sc1 t)) = do decl <- closureConvert tm
                                            sc <- closureConvert t 
                                            return $ IrLet n (convertType ty) decl sc
closureConvert (App _ t1 t2) = --seria un ircall pero how y pq lista en 2do  
-- app y fix ?

convertType :: Ty -> IrTy
convertType NatTy = IrInt
convertType (FunTy _ _) = IrClo

runCC' :: [Decl TTerm] -> M [IrDecl]
runCC' [] = return []
runCC' (Decl{}:ds) = do
    tt <- closureConvert declBody
    return $ IrVal declName __ tt

runCC :: [Decl TTerm] -> [IrDecl]
runCC mod = 
    let ((vals, _stfinal), accs) = runWriter (runStateT (runCC' mod) 0) in
        accs ++ vals -- ver orden