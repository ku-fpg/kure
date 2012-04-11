{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module FibKURE where

import Control.Applicative
import Data.Monoid

import Language.KURE
import FibAST

instance Term Arith where  
  type Generic Arith = Arith
  
instance Walker () Maybe Arith where
  
  allR r = rewrite $ \ c e -> case e of
                                 Lit n     -> pure (Lit n)
                                 Add e1 e2 -> liftA2 Add (apply r c e1) (apply r c e2)
                                 Sub e1 e2 -> liftA2 Sub (apply r c e1) (apply r c e2)
                                 Fib e     -> liftA  Fib (apply r c e)
                                         
  crushT t = translate $ \ c e -> case e of                     
                                    Lit n     -> pure mempty
                                    Add e1 e2 -> liftA2 mappend (apply t c e1) (apply t c e2)
                                    Sub e1 e2 -> liftA2 mappend (apply t c e1) (apply t c e2)
                                    Fib e     -> apply t c e
