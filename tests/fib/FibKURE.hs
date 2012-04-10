{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module FibKURE where

import Control.Applicative
import Data.Monoid

import Language.KURE
import FibAST


instance Injection Arith Arith where  
  inject = id
  retract = Just

instance Term Arith where  
  type Generic Arith = Arith
  
instance Walker () Maybe Arith where
  
  allR gr = rewrite $ \ c e -> case e of
                                 Lit n     -> pure (Lit n)
                                 Add e1 e2 -> liftA2 Add (apply (extractR gr) c e1) (apply (extractR gr) c e2)
                                 Sub e1 e2 -> liftA2 Sub (apply (extractR gr) c e1) (apply (extractR gr) c e2)
                                 Fib e     -> liftA  Fib (apply (extractR gr) c e)
                                         
  crushT t = translate $ \ c e -> case e of                     
                                    Lit n     -> pure mempty
                                    Add e1 e2 -> liftA2 mappend (apply (extractT t) c e1) (apply (extractT t) c e2)
                                    Sub e1 e2 -> liftA2 mappend (apply (extractT t) c e1) (apply (extractT t) c e2)
                                    Fib e     -> apply (extractT t) c e
