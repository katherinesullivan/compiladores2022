module Test where 

import Common
import Elab
import MonadFD4
import Global
import Parse

test = runFD4 
            (elabDecl (
                        case runP program "type Jorge = Nat -> Nat" "" of
                            Left e -> error ("fallo")
                            Right (x:xs) -> x
                    )) 
            (Conf False Interactive)