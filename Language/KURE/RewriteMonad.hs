-- |
-- Module: Language.KURE.RewriteMonad 
-- Copyright: (c) 2006-2008 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This is the definition of the monad inside KURE.

module Language.KURE.RewriteMonad 
        ( RewriteM      -- abstract
        , RewriteStatusM(..)
        , runRewriteM
        , failM
        , catchM
        , chainM
        , liftQ
        , markM
        , transparently
        , getDecsM
        , mapDecsM
        ) where 


import Control.Monad
import Data.Monoid

------------------------------------------------------------------------------

data RewriteM m dec exp = 
   RewriteM { -- | 'runRewriteM' runs the 'RewriteM' monad, returning a status.
              runRewriteM :: dec -> m (RewriteStatusM dec exp) 
             }

data IdStatus = EmptyId | IsId | NotId

instance Monoid IdStatus where
        mempty = EmptyId
        
        mappend EmptyId y = y
        mappend x EmptyId = x
        mappend IsId IsId = IsId
        mappend _  _      = NotId

data RewriteStatusM dec exp
     = RewriteReturnM exp !(Maybe dec) !IdStatus      -- ^ a regular success
     | RewriteFailureM String           -- ^ a real failure
--     | RewriteIdM exp                 -- ^ identity marker on a value


-- TWO possible ways of thinking about rewriting:

-- C1 (e1) => C2 (C1 (e2)) => C3 (C2 (C1 (e3))) -- matches the *writer* like status
-- C1 (e1) => C1 (C2 (e2)) => C1 (C2 (C3 (e3))) -- will require mergeing??

instance (Monoid dec,Monad m) => Monad (RewriteM m dec) where
   return e = RewriteM $ \ _ -> return $ RewriteReturnM e Nothing EmptyId
   (RewriteM m) >>= k = RewriteM $ \ dec -> do
           r <- m dec
           case r of
             RewriteReturnM r1 ds ids -> do
                r2 <- runRewriteM (k r1) dec
                return $ case r2 of
                 RewriteReturnM e' ds' ids' -> RewriteReturnM e' (ds' `mappend` ds) (ids' `mappend` ids)
                 RewriteFailureM msg        -> RewriteFailureM msg
             RewriteFailureM msg        -> return $ RewriteFailureM msg

   fail msg = RewriteM $ \ _ -> return $ RewriteFailureM msg

instance (Monoid dec,Monad m) => Functor (RewriteM m dec) where
  fmap f m = liftM f m

-- | 'liftQ' lets you tunnel into the inner monad, because 'RewriteM' is actually monad transformer.
liftQ :: (Monad m,Monoid dec) =>  m a -> RewriteM m dec a   
liftQ m = RewriteM $ \ _ -> do r <- m
                               return $ RewriteReturnM r mempty mempty

-- | 'failM' is our basic failure, with a String message.
failM :: (Monad m, Monoid dec) => String -> RewriteM m dec a
failM msg = RewriteM $ \ _ -> return $ RewriteFailureM msg

-- | 'catchM' catches failures, and tries a second monadic computation.
catchM :: (Monad m) => RewriteM m dec a -> (String -> RewriteM m dec a) -> RewriteM m dec a
catchM (RewriteM m1) m2 = RewriteM $ \ dec -> do
        r <- m1 dec
        case r of
          RewriteReturnM {}    -> return r 
          RewriteFailureM msg  -> runRewriteM (m2 msg) dec
          
          
-- | 'chainM' executes the first argument then the second, much like '>>=',
-- except that the second computation can see if the first computation was an identity or not.
-- Used to spot when a rewrite succeeded, but was the identity.

chainM :: (Monoid dec,Monad m) 
       => (RewriteM m dec b) 
       -> (Bool -> b -> RewriteM m dec c)
       -> RewriteM m dec c
chainM m k = RewriteM $ \ dec -> do
        r <- runRewriteM m dec
        case r of
          RewriteReturnM a ds ids -> 
                do r2 <- runRewriteM (k (isId ids) a) (case ds of
                                                         Nothing -> dec
                                                         Just ds2 -> ds2 `mappend` dec)
                   case r2 of
                     RewriteReturnM a' ds' ids' ->
                         return $ RewriteReturnM a' (ds' `mappend` ds) (ids' `mappend` ids)
                     RewriteFailureM msg -> return $ RewriteFailureM msg
          RewriteFailureM msg        -> return $ RewriteFailureM msg -- and still fail 
  where
          isId NotId = False
          isId _     = True
          
-- | 'markM' is used to mark a monadic rewrite as a non-identity,
-- unless the congruence flag is set.
markM :: (Monad m) => RewriteM m dec a -> RewriteM m dec a
markM (RewriteM m) = RewriteM $ \ dec -> do
        r <- m dec
        case r of
          RewriteReturnM a ds EmptyId -> return $ RewriteReturnM a ds NotId
          RewriteReturnM a ds IsId    -> return $ RewriteReturnM a ds EmptyId
          RewriteReturnM a ds ids     -> return $ RewriteReturnM a ds ids
          RewriteFailureM msg         -> return $ RewriteFailureM msg
          
-- | 'transparently' sets the congruence flag, such that if the
-- monadic action was identity preserving, then a 'markM' does
-- not set the non-indentity flag.
        
transparently :: (Monad m) => RewriteM m dec a -> RewriteM m dec a
transparently (RewriteM m) = RewriteM $ \ dec -> do
        r <- m dec
        case r of
          RewriteReturnM a ds EmptyId -> return $ RewriteReturnM a ds IsId
          RewriteReturnM a ds ids     -> return $ RewriteReturnM a ds ids
          RewriteFailureM msg         -> return $ RewriteFailureM msg


-- | 'getDecsM' reads the local environment
getDecsM :: (Monad m, Monoid dec) => RewriteM m dec dec
getDecsM = RewriteM $ \ dec -> return $ RewriteReturnM dec mempty mempty

-- | 'mapDecs' changes the local environment, inside a local monadic invocation.
mapDecsM :: (Monad m, Monoid dec) => (dec -> dec) -> RewriteM m dec a -> RewriteM m dec a
mapDecsM fn (RewriteM m) = RewriteM $ \ dec -> m (fn dec)
