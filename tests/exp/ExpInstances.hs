{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module ExpInstances where

import Language.KURE
import Exp

import Data.Monoid
import Control.Applicative

instance Term Exp where
   type Generic Exp = Exp  -- Exp is its own Generic root.
  
instance Walker Context ExpM Exp where
  
   allR r = rewrite $ \ c e -> case e of 
                                 Var v     -> pure (Var v)
                                 App e1 e2 -> liftA2 App (apply r c e1) (apply r c e2)
                                 Lam n e   -> liftA (Lam n) (apply r (n:c) e)

   crushT t = translate $ \ c e -> case e of
                                     Var v     -> pure   mempty
                                     App e1 e2 -> liftA2 mappend (apply t c e1) (apply t c e2)
                                     Lam n e   -> apply t (n:c) e
