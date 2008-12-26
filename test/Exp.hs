{-# LANGUAGE TypeFamilies #-}

module Exp where

import Language.KURE
        
type Name = String
data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name
   deriving Show

instance Term Exp where
  type Generic Exp = Exp  -- Exp is its own Generic root.
  inject    = id
  select e  = return e

-- examples
e1 = Var "x"
e2 = Var "y"
e3 = Lam "x" e1
e4 = Lam "x" e2
e5 = App e1 e2
e6 = App e3 e4
e7 = App e4 e6
