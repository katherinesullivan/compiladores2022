module ClosureConvert where

import Lang
import IR
import Control.Monad.State
import Control.Monad.Writer

type M a = StateT Int (Writer [IrDecl]) a

closureConvert :: Term -> M Ir

closureConvert (V _ (Bound i)) = return
closureConvert (V _ (Free n)) = return $ IrVar n
closureConvert (V _ (Global n)) = return $ IrGlobal n
closureConvert (Const _ c) = return $ IrConst c
closureConvert (Lam (_,fty) n _ s) = do fr <-  get
                                        put $ fr + 1
                                        frName <- "__f" ++ show fr

                                        ird <- IrFun frName (convertType fty) 

closureConvert (BinaryOp _ op t1 t2) = return $ IrBinaryOp op (closureConvert t1) (closureConvert t2)
closureConvert (IfZ _ t1 t2 t3) = return $ IrIfZ (closureConvert t1) (closureConvert t2) (closureConvert t3)
closureConvert (Print _ s t1) = return $ IrPrint s t1


convertType :: Ty -> IrTy
convertType NatTy = IrInt
convertType FunTy = IrClo

runCC' :: [Decl TTerm] -> M [IrDecl]
runCC' [] = return []
runCC' (Decl{}:ds) = do
    tt <- closureConvert declBody
    return $ IrVal declName __ tt

runCC :: [Decl TTerm] -> [IrDecl]
runCC mod = 
    let ((vals, _stfinal), accs) = runWriter (runStateT (runCC' mod) 0) in
        accs ++ vals -- ver orden