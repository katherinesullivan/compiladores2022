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

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: STerm -> Term
elab (SLam p [] _) = failPosFD4 p "Funcion sin suficientes argumentos"
elab (SLet p True [x] _ _) = failPosFD4 p "Let rec sin suficientes argumentos"
elab (SLet p _ [] _ _) = failPosFD4 p "Let sin suficientes argumentos"
elab (SFix p [] _) = failPosFD4 p "Fix sin suficientes argumentos"
elab (SFix p [x] _) = failPosFD4 p "Fix sin suficientes argumentos"
elab _ = elab' []

elab' :: [Name] -> STerm -> Term
elab' env (SV p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  V p (Free v)
    else V p (Global v)

elab' _ (SConst p c) = Const p c
elab' env (SLam p ((v,ty):[]) t) = Lam p v ty (close v (elab (v:env) t))
elab' env (SLam p ((v,ty):xs) t) = Lam p v ty (close v (elab' (v:env) (SLam p xs t)))
elab' env (SFix p ((f,fty):(x,xty):xs) t) = Fix p f fty x xty (close2 f x (elab (x:f:env) t'))
                                            where t' = if xs == [] then t else SLam p xs t
elab' env (SIfZ p c t e) = IfZ p (elab env c) (elab env t) (elab env e)
-- Operadores binarios
elab' env (SBinaryOp i o t u) = BinaryOp i o (elab env t) (elab env u)
-- Operador Print
elab' env (SPrint i str) = Lam i "x" NatTy (close "x" (Print i str (V i (Free "x"))))
-- Aplicaciones generales
elab' env (SApp p (SPrint i str) a) = Print i str (elab env a)
elab' env (SApp p h a) = App p (elab env h) (elab env a)
elab' env (SLet p False ((v,vty):xs) def body) = Let p v (buildtype xs vty) (elab env (ISLam p xs def)) (close v (elab (v:env) body))
                                                 where ISLam p [] def = def
                                                       ISLam p xs def = SLam p xs def
elab' env (SLet p True ((v,vty):(x,xty):xs) def body) | xs == [] = Let p v (FunTy xty vty) (Fix v (FunTy xty vty) x xty (close2 v x (elab env def))) (close v (elab (v:x:env) body))
                                                      | otheriwise = elab env (SLet p True ((v,buildtype xs t):(x,xty):[]) (SLam p xs def) body)
                                                                     
buildtype :: [(Name,Ty)] -> Ty
buildtype [] t = t
buildtype ((_,y):ys) t = FunTy y (buildtype ys t)


elabDecl :: SDecl STerm -> Decl Term
elabDecl (SDecl p False v vty [] t) = Decl p v vty t
elabDecl (SDecl p False v vty ((x,xty):xs) t) = Decl p v (buildtype xs xty) (elab ())
