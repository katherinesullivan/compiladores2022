module Optimizer where
{-
import Lang
import Subst
import MonadFD4
import Eval
import GHC.Generics (Generic1(to1))

optimize :: MonadFD4 m => Decl TTerm -> m (Decl TTerm)
optimize (Decl p n ty t) = go t 30 where
    go t1 0 = return t
    go t1 n = do t2 <- cp t
                 t3 <- cf t1
                 go t3 (n-1)  

fresh :: MonadFD4 m => m Name
fresh = do i <- getAndUpdateFresh
           return ("__x" ++ show i)


visit :: MonadFD4 m => (TTerm -> m TTerm) -> TTerm -> m TTerm
visit f t = case t of
    BinaryOp i op t1 t2 -> do
        t1' <- visit f t1
        t2' <- visit f t2
        f $ BinaryOp i op t1' t2'
    App i t1 t2 -> do
        t1' <- visit f t1
        t2' <- visit f t2
        f $ App i t1' t2'
    Print i s t1 -> do
        t1' <- visit f t1
        f $ Print i s t1'
    IfZ i t1 t2 t3 -> do
        t1' <- visit f t1
        t2' <- visit f t2
        t3' <- visit f t3
        f $ IfZ i t1' t2' t3'
    Lam i n ty sc -> do
        fr <- fresh
        sc1 <- open fr sc
        sc2 <- visit f sc1
        sc3 <- close fr sc2
        f $ Lam i n ty sc3
    Let i n ty t1 sc -> do
        t1' <- visit f t1
        fr <- fresh
        sc1 <- open fr sc
        sc2 <- visit f sc1
        sc3 <- close fr sc2
        return $ Let i n ty t1' sc3
    Fix i nf tyf nx tyx sc -> do 
        fr <- fresh
        sc1 <- open fr sc
        sc2 <- visit f sc1
        sc3 <- close fr sc2  
        return $ Fix i nf tyf nx tyx sc3
    t1 -> f t1

cf :: MonadFD4 m => TTerm -> m TTerm
cf t = visit cf1 t where
    cf1 (BinaryOp i op (Const _ (CNat n1)) (Const _ (CNat n2))) = return $ Const i (CNat (semOp op n1 n2))
    cf1 (IfZ i (Const _ (CNat n1)) t2 t3) = return $ if n1 == 0 then t2 else t3
    cf1 t' = return t'


cp :: MonadFD4 m => TTerm -> m TTerm
cp t = visit cp1 t where
    cp1 (Let i n ty t'@(Const _ (CNat _)) sc) = return $ subst t' sc
    cp1 t' = return t'

-- se podrian componer cf1 y cp1 y llamar a visit con esa funcion compuesta para desminuir la cantidad de iteraciones que se necesitan
-}