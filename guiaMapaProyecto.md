## Ejercicio 1

a) El AST del lenguaje se encuentra definido en Lang.hs

b) Tanto Term como TTerm representan terminos del lenguaje, ambos formandolos con un argumento Tm (el termino en si), un argumento Pos (posicion en el archivo) y un argumento Var (variables -- un tipo que representa la union disjunta de indices de de Bruijn, nombres locales (nombres que surgen de abrir un t ́ermino), y nombres globales (nombres de definiciones top-level)).

La diferencia radica en que TTerm tambien cuenta con el tipo del termino en el que estamos posicionados. (creo)

```
-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data Tm info var =
    V info var
  | Const info Const
  | Lam info Name Ty (Scope info var)
  | App info (Tm info var) (Tm info var)
  | Print info String (Tm info var)
  | BinaryOp info BinaryOp (Tm info var) (Tm info var)
  | Fix info Name Ty Name Ty (Scope2 info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  | Let info Name Ty (Tm info var)  (Scope info var)
  deriving (Show, Functor)


type Term = Tm Pos Var       -- ^ 'Tm' con índices de De Bruijn como variables ligadas, y nombres para libres y globales, guarda posición
type TTerm = Tm (Pos,Ty) Var -- ^ 'Tm' con índices de De Bruijn como variables ligadas, y nombres para libres y globales, guarda posición y tipo
```

## Ejercicio 2

Para generar un comentario en FD4 usamos el marcador "#". Para agregar otra manera de comentar o cambiar la existente debemos dirigirnos a Parse.hs y dentro de langDef cambiar el simbolo que representa commentLine.

## Ejercicio 3

```
-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (Decl STerm) STerm)
declOrTm =  try (Left <$> decl) <|> (Right <$> expr)
```

Como la funcion Parse.declOrTm parsea o bien una declaracion top-level o un termino, necesitamos que el operador choice ejecute su segunda opcion aun cuando el primer parser falla habiendo consumido input; como el operador try permite que se ejecute el parser que le es pasado como parametro y hace como que no consumio entrada necesitamos el try para realizar el parseo correcto. 

## Ejercicio 4

El resultado de evaluar 2-2-2 es 0, puesto que se evalua (2-2)-2 y estamos trabajando con naturales.

## Ejercicio 5

El argumento extra de elab' ([Name]) representa el environment del termino, es decir, las variables locales. Necesitamos poder reconocer estas porque la idea de elab es que transforne en indices de de Bruijin solo a las variables ligadas. 

Son de interes estos 3 comportamientos de elab'

```
elab' :: [Name] -> STerm -> Term
elab' env (SV p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  V p (Free v)
    else V p (Global v)

elab' env (SLam p (v,ty) t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (SFix p (f,fty) (x,xty) t) = Fix p f fty x xty (close2 f x (elab' (x:f:env) t))
```

## Ejercicio 6

La clase Monad FD4 importa todas las funciones miembro de sus superclases (MonadIO, MonadState GlEnv, MonadError Error, MonadReader Conf).

Las mónadas @m@ de esta clase cuentan con las operaciones:
   - @ask :: m Conf@
   - @get :: m GlEnv@
   - @put :: GlEnv -> m ()@
   - @throwError :: Error -> m a@
   - @catchError :: m a -> (Error -> m a) -> m a@
   - @liftIO :: IO a -> m a@

y otras operaciones derivadas de ellas, como por ejemplo
   - @modify :: (GlEnv -> GlEnv) -> m ()@
   - @gets :: (GlEnv -> a) -> m a@  

## Ejercicio 7

```
-- `open n t` reemplaza la primera variable ligada
-- en `t` (que debe ser un Scope con una sola variable que 
-- escapa al término) por el nombre libre `n`.
-- La variable Bound 0 pasa a ser Free n. El nombre `n`
-- debe ser fresco en el término para que no ocurra shadowing.
open :: Name -> Scope info Var -> Tm info Var
open nm (Sc1 t) = varChanger (\_ p n -> V p (Free n)) bnd t
   where bnd depth p i | i <  depth = V p (Bound i)
                       | i == depth =  V p (Free nm)
                       | otherwise  = abort "open: M is not LC"
```

Se necesita contar con un abort puesto que podria suceder que encontremos un indice de de Bruijin incorrecto (mayor al posible en el actual contexto), esto significaria que no estamos trabajando con terminos LC (puesto que si lo estuviesemos haciendo solo podriamos tener un indice que escapa -0, entrando en el caso i == depth-).

## Ejercicio 8

Al ejecutar 

    let v = print "Prueba" (1+1) 

Se imprimira Prueba2 (el string = el valor al que evalua), se agrega la variable v al environment y esta pasara a evaluar a 2.

Luego, cuando se utilice v no se imprimira otra vez el valor.

Al ejecutar 

    let f = fun (x:Nat) -> print "Prueba 2" (x + x)

Sucedera lo mismo con la salvedad de que ahora cada vez que se utilice la variable f se efectuara la impresion correspondiente porque siempre se evalua la sentencioa print.

## Ejercicio 9

La salida al evaluar 

    print "Hola " (print "mundo!" 2)

sera:

    mundo!2
    Hola 2
    2 : Nat

Esto porque primero se evalua el termino entre parentesis y luego el print de afuera, para por ultimo devolver el valor al que evalua la expresion en su totalidad.

## Ejercicio 10

El modulo PPrint exporta las siguientes funciones:

 - pp :: MonadFD4 m => TTerm -> m String : pretty printer de terminos
 - ppTy :: Ty -> String : pretty printer de tipos : pretty printer de tipos
 - ppName :: Name -> String : pretty printer de nombres
 - ppDecl :: MonadFD4 m => Decl TTerm -> m String : pretty printer de declaraciones

## Ejercicio 11

Al abrir un termino con un nombre n es necesario que este no se encuentre libre en el mismo. Para asegurarse de que este no suceda la funcion openAll lleva en su primer argumento una lista con los nombres que ya fueron abiertos en el termino. Con asegurarnos de que no se abran nombres que ya fueron abiertos 



