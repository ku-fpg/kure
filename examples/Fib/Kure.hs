{-# LANGUAGE CPP, InstanceSigs, LambdaCase, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fib.Kure (Crumb(..)) where

import Prelude hiding (Left, Right)

import Language.KURE

import Fib.AST

import Control.Monad(liftM, ap)

--------------------------------------------------------------------------------------

data Crumb = LeftChild | RightChild | OnlyChild deriving (Eq,Show)

instance ExtendPath c Crumb => Walker c Arith where
   allR :: MonadCatch m => Rewrite c m Arith -> Rewrite c m Arith
   allR r = modExc (stackStrategyFailure "allR") $
     rewrite $ \ c -> \case
                         Lit n      ->  Lit <$> return n
                         Add e0 e1  ->  Add <$> applyR r (c @@ LeftChild) e0 <*> applyR r (c @@ RightChild) e1
                         Sub e0 e1  ->  Sub <$> applyR r (c @@ LeftChild) e0 <*> applyR r (c @@ RightChild) e1
                         Fib e0     ->  Fib <$> applyR r (c @@ OnlyChild) e0

--------------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 708
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}
#endif

--------------------------------------------------------------------------------------
