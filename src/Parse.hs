{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-|
Module      : Parse
Description : Define un parser de términos FD40 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang hiding (getPos)
import Common
import Text.Parsec hiding (runP,parse)
--import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "rec","fun", "fix", "then", "else","in",
                           "ifz", "print","Nat","type"],
         reservedOpNames = ["->",":","=","+","-"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

tyIdentifier :: P String
tyIdentifier = Tok.lexeme lexer $ do
  c  <- upper
  cs <- many (identLetter langDef)
  return (c:cs)

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P STy
tyatom = (reserved "Nat" >> return NatSTy)
         <|> 
         (do t <- tyIdentifier
             return (DeclSTy t))
         <|>
         parens typeP

typeP :: P STy
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (FunSTy x y))
        <|> tyatom
          
const :: P Const
const = CNat <$> num

printOp :: P STerm
printOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  return (SPrint i str)

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity STerm
binary s f = Ex.Infix (reservedOp s >> return (SBinaryOp NoPos f))

table :: [[Operator String () Identity STerm]]
table = [[binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft]]

expr :: P STerm
expr = Ex.buildExpressionParser table tm

atom :: P STerm
atom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> parens expr
       <|> printOp

-- parsea un par (variable : tipo)
binding :: P (Name, STy)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)

binders :: P [(Name, STy)]
binders = try (do x <- parens binding 
                  xs <- binders
                  return (x:xs)) 
          <|> 
          return []
               

lam :: P STerm
lam = do i <- getPos
         reserved "fun"
         xs <- binders
         reservedOp "->"
         t <- expr
         return (SLam i xs t)

-- Nota el parser app también parsea un solo atom.
app :: P STerm
app = do i <- getPos
         f <- atom
         args <- many atom
         return (foldl (SApp i) f args)

ifz :: P STerm
ifz = do i <- getPos
         reserved "ifz"
         c <- expr
         reserved "then"
         t <- expr
         reserved "else"
         e <- expr
         return (SIfZ i c t e)

fix :: P STerm
fix = do i <- getPos
         reserved "fix"
         xs <- binders
         reservedOp "->"
         t <- expr
         return (SFix i xs t)

letexp :: P STerm
letexp = do
  i <- getPos
  reserved "let"
  isrec <- try (reserved "rec" >> return True) <|> return False
  xs <- (
    try (do v <- var -- para permitir notación amigable en funciones y un binder sin parentesis
            l <- binders
            reservedOp ":"
            ty <- typeP
            return ((v, ty):l)) -- estoy agregando la función con el tipo de retorno (el último, no los intermedios) como primer elemento de la lista
    <|>
    binders) -- para permitir parentesis
  reservedOp "="  
  def <- expr
  reserved "in"
  body <- expr
  return (SLet i isrec xs def body)


-- | Parser de términos
tm :: P STerm
tm = app <|> lam <|> ifz -- <|> printOp 
     <|> fix <|> letexp

-- | Parser de declaraciones
decl :: P (SDecl STerm)
decl = do 
     i <- getPos
     reserved "let"
     isrec <- (try (reserved "rec" >> return True)) <|> return False
     v <- var
     binds <- try binders <|> return []
     ty <- try (do reservedOp ":"
                   typeP)
           <|> return SNoTy
     reservedOp "="
     t <- expr
     return (SDecl i isrec v ty binds t)

-- | Parser de declaraciones de sinonimos de tipo
declSTy :: P (SDecl STerm)
declSTy = do 
       i <- getPos
       reserved "type"
       v <- var
       reservedOp "="
       ty <- typeP
       return (STDecl i v ty)

-- | Parser de programas (listas de declaraciones) 
program :: P [SDecl STerm]
program = many (try decl <|> declSTy) --

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (SDecl STerm) STerm)
declOrTm =  try (Left <$> decl) <|> (Right <$> expr)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP expr s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
