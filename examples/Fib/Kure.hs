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
                         Add e0 e1  ->  Add <$> apply r (c @@ LeftChild) e0 <*> apply r (c @@ RightChild) e1
                         Sub e0 e1  ->  Sub <$> apply r (c @@ LeftChild) e0 <*> apply r (c @@ RightChild) e1
                         Fib e0     ->  Fib <$> apply r (c @@ OnlyChild) e0

--------------------------------------------------------------------------------------

-- I find it annoying that Applicative is not a superclass of Monad.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}

--------------------------------------------------------------------------------------
