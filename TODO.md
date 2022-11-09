# TO DO compilador:

## Syntactic sugar:
 - implementacion de multibinders
 - recordar los nombres de los sinónimos de tipo para pretty-printearlos y devolver mejores errores
 - que el pretty printer resugaree
 - se pueden agregar metadatos para que se resugaree lo justo y necesario (ni siquiera esta propuesto, va a haber que tocar muchas cosas y lo veo medio inalcanzable)
 - testear todo basically

## Maquina CEK
 - should work, falta testear

## La Macchina

 - Implementar la compilación a bytecode y máquina virtual en Haskell, usando el esqueleto provisto. Dado que las instrucciones PRINT y PRINTN imprimen a la consola, la función que ejecuta la máquina debe estar en MonadFD4.
 - Proponer e implementar un esquema de compilación y ejecución para ifz. Pueden agregarse nuevas instrucciones a la máquina.
 - Implementar un esquema de compilación para el operador print usando las instrucciones PRINT y PRINTN.
 - (Opcional) Reduzca el tamaño del bytecode generado usando un solo byte para la codificación de las instrucciones. Adapte la máquina para funcionar con su nuevo formato. Puede también mejorar la codificación de las cadenas usando UTF-8 (en vez de UTF-32).
 - (Opcional) Compare la performance de computar ack 3 11 en la Macchina con la de un programa C nativo. Explique la discrepancia.

 --

## Clausure conversion

Pasada sobre el programa para convertir cada funcion en una creacion de clausura. Para ello, usamos la sintaxis (fun (x:τ ) -> ...)[v1, . . . , vn] para indicar la creacion de una clausura para la funcion dada. Las variables libres de la funcion son reemplazadas por variables distinguidas de la forma ei, las cuales toman los valores vi

llamado rec -> ver lo que es

en padre de todo -> tengo una sola var libre -> agarro lo del llamado rec si es una clausura 

variables libres -> 

## Hoisting

Recorremos todo el termino, reemplazando cada fun por un nombre, y generando una definicion top-level de ese nombre con el mismo cuerpo (tomar el resultado de la conversi ́on de clausuras y convertir las funciones lambda dentro de las creaciones de clausuras en definiciones globales, cada una con algun nombre fresco)
