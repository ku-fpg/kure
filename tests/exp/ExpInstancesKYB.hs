{-# LANGUAGE TypeFamilies, TemplateHaskell, NoMonomorphismRestriction #-}

module ExpInstancesKYB where

import Language.KURE

import Exp

import Data.Monoid
import Control.Monad

type MyGeneric = Exp

-- remember the NoMonomorphismRestriction!
kureYourBoilerplate ''MyGeneric

