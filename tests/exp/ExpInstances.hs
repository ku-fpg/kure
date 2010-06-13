{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module ExpInstances where

import Language.KURE

import Exp

import Data.Monoid
import Control.Monad


type R e = T e e
type T e1 e2 = Translate e1 e2


instance Term Exp where
   type Generic Exp = Exp  -- Exp is its own Generic root.
   inject    = id
   select e  = return e

   allR rr   = appR rr rr <+ lamR rr <+ varR
   crushU rr = appU rr rr <+ lamU rr <+ varU

------------------------------------------------------------------------
--
-- First the guards
--

appG :: R Exp
appG = acceptR $ \ e -> case e of { App {} -> True ; _ -> False }

lamG :: R Exp
lamG = acceptR $ \ e -> case e of { Lam {} -> True ; _ -> False }

varG :: R Exp
varG = acceptR $ \ e -> case e of { Var {} -> True; _ -> False }

------------------------------------------------------------------------
--
-- Then the rewrites and Universals
--


appR :: R Exp 
                              -> R Exp
                              -> R Exp
appR rr1 rr2 = appG >-> rewrite (\ (App e1 e2) -> 
                                liftM2 App (apply rr1 e1) 
                                           (apply rr2 e2)) 

lamR :: R Exp 
                              -> R Exp
lamR rr = lamG >-> rewrite (\ (Lam n e) -> do
                                e' <- apply rr e
                                return $ Lam n e')
                                           
varR :: R Exp
varR = varG

appU :: (Monoid r) => 
                                 T Exp r
                              -> T Exp r
                              -> T Exp r
appU rr1 rr2 = appG >-> translate (\ (App e1 e2) -> 
                                liftM2 mappend (apply rr1 e1) 
                                               (apply rr2 e2)) 

lamU :: (Monoid r) => T Exp r
                              -> T Exp r
lamU rr = lamG >-> translate (\ (Lam n e) -> do
                                e' <- apply rr e
                                return $ e')
                                           
varU :: (Monoid r) => T Exp r
varU = varG >-> translate (\ _ -> return $ mempty)


------------------------------------------------------------------------
--
-- Finally, the pattern matches
--

appP ::(Exp -> Exp -> T Exp r)
                              -> T Exp r
appP f = appG >-> readerT (\ (App e1 e2) -> f e1 e2) 

lamP ::  (Name -> Exp -> T Exp r)
                              -> T Exp r
lamP f = lamG >-> readerT (\ (Lam n e) -> f n e)

varP :: (Name -> T Exp r)
                              -> T Exp r
varP f = varG >-> readerT (\ (Var n) -> f n)

------------------------------------------------------------------------
