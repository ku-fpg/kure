{-# LANGUAGE MultiParamTypeClasses #-}

module Fib.Kure where

import Language.KURE
import Fib.AST

import Control.Monad(liftM, ap)

--------------------------------------------------------------------------------------

instance Walker AbsolutePath Arith where
-- allR :: MonadCatch m => Rewrite AbsolutePath m Arith -> Rewrite AbsolutePath m Arith
   allR r = prefixFailMsg "allR failed: " $
     rewrite $ \ c e ->
     let c0 = extendAbsPath 0 c
         c1 = extendAbsPath 1 c
      in case e of
           Lit n      ->  Lit <$> return n
           Add e0 e1  ->  Add <$> apply r c0 e0 <*> apply r c1 e1
           Sub e0 e1  ->  Sub <$> apply r c0 e0 <*> apply r c1 e1
           Fib e0     ->  Fib <$> apply r c0 e0

--------------------------------------------------------------------------------------

-- I find it annoying that Applicative is not a superclass of Monad.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}

--------------------------------------------------------------------------------------
