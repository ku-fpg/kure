{-# LANGUAGE ExistentialQuantification, Rank2Types, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Main where

import Language.KURE
import Language.KURE.Term as T

import Data.Monoid
import Control.Monad
import Data.List

import Exp
import Id

type R e = Rewrite Id () e
type T e1 e2 = Translate Id () e1 e2

main = do
	let es1 = [e1,e2,e3,e4,e5,e6,e7]
	sequence_ [ print e | e <- es1]

	let frees :: Exp -> Id [Name]
	    frees exp = do Right (fs,b) <- runTranslate freeExp () exp
			   return $ nub fs
	let e_frees = map (runId . frees) es1
	sequence_ [ print e | e <- e_frees]
        

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
                                transparently $ 
                                liftM2 App (apply rr1 e1) 
                                           (apply rr2 e2)) 

lamR :: R Exp 
                              -> R Exp
lamR rr = lamG >-> rewrite (\ (Lam n e) -> 
                                transparently $ do
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

instance Walker Id () Exp where
   allR rr = appR rr rr <+ lamR rr <+ varR
   crushU rr = appU rr rr <+ lamU rr <+ varU

------------------------------------------------------------------------

freeExp :: T Exp [Name]
freeExp = lambda <+ var <+ crushU freeExp
  where
          var    = varG >-> translate (\ (Var v) -> return [v])
          lambda = lamG >-> translate (\ (Lam n e) -> do
                frees <- apply freeExp e
                return (nub frees \\ [n]))
