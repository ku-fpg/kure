{-# LANGUAGE InstanceSigs, LambdaCase, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Fib.Kure (Crumb(..)) where

import Prelude hiding (Left, Right)

import Language.KURE

import Fib.AST

import Control.Monad(liftM, ap)

--------------------------------------------------------------------------------------

data Crumb = LeftChild | RightChild | OnlyChild deriving (Eq,Show)

instance ExtendPath c Crumb => Walker c Arith where
   allR :: MonadCatch m => Rewrite c m Arith -> Rewrite c m Arith
   allR r = prefixFailMsg "allR failed: " $
     rewrite $ \ c -> \case
                         Lit n      ->  Lit <$> return n
                         Add e0 e1  ->  Add <$> applyR r (c @@ LeftChild) e0 <*> applyR r (c @@ RightChild) e1
                         Sub e0 e1  ->  Sub <$> applyR r (c @@ LeftChild) e0 <*> applyR r (c @@ RightChild) e1
                         Fib e0     ->  Fib <$> applyR r (c @@ OnlyChild) e0

--------------------------------------------------------------------------------------
