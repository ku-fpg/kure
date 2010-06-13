{-# LANGUAGE TypeFamilies, TemplateHaskell, NoMonomorphismRestriction
  #-}

module ExpInstances where

import Language.KURE

import Exp

import Data.Monoid
import Control.Monad

type R e = T e e
type T e1 e2 = Translate e1 e2

type MyGeneric = Exp

-- remember the NoMonomorphismRestriction!
kureYourBoilerplate ''MyGeneric

