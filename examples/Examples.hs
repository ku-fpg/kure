module Examples where

import Fib.Examples as F
import Lam.Examples as L
import Expr.Examples as E

---------------------------------------------------------------

main :: IO ()
main = do ppTest "Fib" F.checkTests
          ppTest "Lam" L.checkTests
          ppTest "Expr" E.checkTests

ppTest :: String -> Bool -> IO ()
ppTest n b = putStrLn (n ++ " examples: tests " ++ ppBool b)

ppBool :: Bool -> String
ppBool True  = "passed"
ppBool False = "failed"

---------------------------------------------------------------
