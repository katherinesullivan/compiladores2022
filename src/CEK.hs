module CEK where

import Lang
import MonadFD4
import Subst ( close, close2 )
import Eval ( semOp )

data Val = N Int | C Clos

data Env = [Val]

data Clos = CLam Env Name Ty TTerm | CFix Env Name Ty Name Ty TTerm

data Frame = App1 Env TTerm -- ρ·□t
           | App2 Clos -- clos □
           | Op1 BinaryOp Env TTerm -- ρ·□ `op` t
           | Op2 BinaryOp Val -- v `op` □
           | FPrint String -- print str □ v `op` □
           | IfzThenElse Env TTerm TTerm -- ρ·ifz □ then t else e \
           | FLet Env Name TTerm

data Kont = [Frame]


seek  :: MonadFD4 m => TTerm -> Env -> Kont -> m Val
seek (Print i n t) env k = seek t env ((FPrint n):k)
seek (BinaryOp i op t1 t2) env k = seek t1 ((Op1 op env t2):k)
seek (IfZ i c t1 t2) env k = seek c env ((IfzThenElse env t1 t2):k)
seek (App i t1 t2) env k = seek t1 env ((App1 env t2):k)
seek (V i (Global n)) env k = do val <- lookupDecl n
                                   case val of
                                        Nothing -> failFD4 $ "Error de ejecución: variable no declarada: " ++ ppName nm
                                        Just t -> seek t env k
seek (V i (Bound i)) env k = destroy (N (env[i])) k
seek (V _ (Free _)) _ _ = failFD4 $ "Error en ejecución: no puede haber variables libres."
seek (Const i (CNat i)) env k = destroy (N i) k
seek (Lam i n ty (Sc1 t)) env k = destroy (C (CLam env n ty t)) k
seek (Fix i f fty x xty (Sc2 t)) = destroy (C (CFix env f fty x xty t)) k
seek (Let i x ty t1 (Sc1 t2)) env k = search t1 env ((FLet env x t2):k)


-- Cualquier otro caso sería un termino mal tipado y no pasaria el typeChecker.
destroy :: MonadFD4 m => Val -> Kont -> m Val
destroy v [] = return v
destroy v ((FPrint s):k) = do printFD4 $ s ++ v
                              destroy v k
destroy n@(N i) ((Op1 op env t):k) = seek t env ((Op2 op n):k)
destroy (N n2) ((Op2 op (N n1)):k) = destroy (N (semOp op n1 n2)) k
destroy (N 0) ((IfzThenElse env t1 t2):k) = seek t1 env k
destroy (N _) ((IfzThenElse env t1 t2):k) = seek t2 env k
destroy (C clos) ((App1 env t):k) = seek t env ((App2 clos):k)
destroy v ((App2 (CLam env x ty t)):k) = seek t (v:env) k
destroy v ((App2 c@(CFix env f fty x xty t)):k) = seek t ((C c):v:env) k -- revisar el orden en el que agg al entorno, depende del comportamiento del resto de funciones
destroy v ((FLet env x t):k) = seek t (v:env) k


val2tterm :: Val -> Pos -> Ty -> TTerm
val2tterm (N i) p ty = Const (p,ty) i
val2tterm (C (CLam env x xty tt)) p ty = Lam (p,ty) x xty (close x tt) -- es correcto usar el close aca? NO
val2tterm (C (CFix env f fty x xty tt)) p ty = Fix (p,ty) f fty x xty (close2 f x tt) -- idem ^^