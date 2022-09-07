{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@STerm) a locally closed (@Term@)
-}

module Elab ( elab, elabDecl) where

import Lang
import Subst
import MonadFD4

elabTypes :: MonadFD4 m => STy -> m Ty
elabTypes NatSTy = return NatTy
elabTypes (FunSTy x y) = do x' <- elabTypes x 
                            y' <- elabTypes y
                            return (FunTy x' y')
elabTypes (DeclSTy n) = do t <- lookupDeclTy n
                           case t of
                            Nothing -> failFD4 "Sinónimo de tipo no definido"
                            Just ty -> return ty 


-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: MonadFD4 m => STerm -> m Term
elab = elab' []

elab' :: MonadFD4 m => [Name] -> STerm -> m Term
elab' env (SV p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  return $ V p (Free v)
    else return $ V p (Global v)
elab' _ (SConst p c) = return $ Const p c
elab' _ (SLam p [] _) = failPosFD4 p "Funcion sin suficientes argumentos"
elab' env (SLam p [(v,ty)] t) = do t' <- elab' (v:env) t
                                   ty' <- elabTypes ty
                                   return $ Lam p v ty' (close v t')
elab' env (SLam p ((v,ty):xs) t) = do t' <- elab' (v:env) (SLam p xs t)
                                      ty' <- elabTypes ty
                                      return $ Lam p v ty' (close v t')
elab' _ (SFix p [] _) = failPosFD4 p "Fix sin suficientes argumentos"
elab' _ (SFix p [x] _) = failPosFD4 p "Fix sin suficientes argumentos"
elab' env (SFix p ((f,fty):(x,xty):xs) t) = do t'' <- elab' (x:f:env) t'
                                               fty' <- elabTypes fty
                                               xty' <- elabTypes xty
                                               return $ Fix p f fty' x xty' (close2 f x t'')
                                            where t' = if xs == [] then t else SLam p xs t
elab' env (SIfZ p c t e) = do c' <- elab' env c
                              t' <- elab' env t
                              e' <- elab' env e
                              return $ IfZ p c' t' e'
-- Operadores binarios
elab' env (SBinaryOp i o t u) = do t' <- elab' env t
                                   u' <- elab' env u
                                   return $ BinaryOp i o t' u'
-- Operador Print
elab' env (SPrint i str) = return $ Lam i "x" NatTy (close "x" (Print i str (V i (Free "x"))))
elab' env (SApp p (SPrint i str) a) = do a' <- elab' env a
                                         return $ Print i str a'
-- Aplicaciones generales
elab' env (SApp p h a) = do h' <- elab' env h
                            a' <- elab' env a
                            return $ App p h' a'
elab' _ (SLet p _ [] _ _) = failPosFD4 p "Let sin suficientes argumentos"
elab' env (SLet p False ((v,vty):xs) def body) = do def' <- elab' env (iSLam p xs def)
                                                    body' <- elab' (v:env) body
                                                    nty <- elabTypes (buildtype xs vty)
                                                    return $ Let p v nty def' (close v body')
                                                 where iSLam i [] t = t
                                                       iSLam i ys t = SLam i ys t
elab' _ (SLet p True [x] _ _) = failPosFD4 p "Let rec sin suficientes argumentos"
elab' env (SLet p True ((v,vty):(x,xty):xs) def body) | xs == [] = do def' <- elab' env def
                                                                      body' <- elab' (v:x:env) body
                                                                      vty' <- elabTypes vty
                                                                      xty' <- elabTypes xty
                                                                      return $ Let p v (FunTy xty' vty') (Fix p v (FunTy xty' vty') x xty' (close2 v x def')) (close v body')
                                                      | otherwise = elab' env (SLet p True [(v,buildtype xs vty),(x,xty)] (SLam p xs def) body)
                                                                     
buildtype :: [(Name,STy)] -> STy -> STy
buildtype [] t = t
buildtype ((_,y):ys) t = FunSTy y (buildtype ys t)


elabDecl :: SDecl STerm -> Decl Term
elabDecl (SDecl p False v vty [] t) = Decl p v vty (elab t)
elabDecl (SDecl p False v vty ((x,xty):xs) t) = Decl p v (buildtype xs (FunTy xty vty)) (elab (SLam p ((x,xty):xs) t))
elabDecl (SDecl p True v vty [] t) = failPosFD4 p "Declaración recursiva sin suficientes argumentos"
elabDecl (SDecl p True v vty ((x,xty):xs) t) | xs == [] = Decl p v (FunTy xty vty) (elab (Fix p v (FunTy xty vty) x xty (close2 v x (elab' [v,x] t))))
                                             | otherwise = elabDecl (SDecl p True v (buildType xs vty) [(x,xty)] (SLam p xs t))