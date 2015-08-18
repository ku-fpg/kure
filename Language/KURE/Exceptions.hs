{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Language.KURE.MonadCatch
-- Copyright: (c) 2012--2015 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: beta
-- Portability: ghc
--
-- This module provides KURE-specific 'Exception' idioms.
module Language.KURE.Exceptions
        ( -- ** Exceptions
          showKureExc
        , NodeMismatch(..)
        , nodeMismatch
        , displayNodeMismatch
        , StrategyFailure(..)
        , strategyFailure
        , stackStrategyFailure
        , toStrategyFailure
        , displayStrategyFailure
        , ConditionalFailure(..)
        , conditionalFailure
        , displayConditionalFailure
        ) where

import Control.Exception (Exception(..), SomeException(..))
import Data.Typeable (Typeable)

-- | A node mismatch exception type, containing the name of the node that
--   was mismatched.
newtype NodeMismatch = NodeMismatch String
  deriving (Eq, Show, Typeable)

instance Exception NodeMismatch where
#if __GLASGOW_HASKELL__ >= 710
    displayException = displayNodeMismatch
    {-# INLINE displayException #-}
#endif

-- | Construct a 'NodeMismatch' from a node name.
nodeMismatch :: String -> NodeMismatch
nodeMismatch = NodeMismatch
{-# INLINE nodeMismatch #-}

-- | Show a 'NodeMismatch' in a human-friendly way.
displayNodeMismatch :: NodeMismatch -> String
displayNodeMismatch (NodeMismatch n) = "node mismatch, " ++ n
{-# INLINE displayNodeMismatch #-}

-- | A strategy failure exception type, containing the name of the failed
--   strategy and, if possible, the reason why it failed.
data StrategyFailure = StrategyFailure String (Maybe SomeException)
  deriving (Show, Typeable)

instance Exception StrategyFailure where
#if __GLASGOW_HASKELL__ >= 710
    displayException = displayStrategyFailure
    {-# INLINE displayException #-}
#endif

-- | Construct a 'StrategyFailure' from a strategy name with no explanation
--   as for why it failed.
strategyFailure :: String -> StrategyFailure
strategyFailure s = StrategyFailure s Nothing
{-# INLINE strategyFailure #-}

-- | Construct a 'StrategyFailure' from a strategy name and an explanation
--   as for why it failed.
stackStrategyFailure :: String -> SomeException -> StrategyFailure
stackStrategyFailure s = StrategyFailure s . Just
{-# INLINE stackStrategyFailure #-}

-- | Construct a 'StrategyFailure' from a strategy name and an explanation
--   as for why it failed. Unlike 'stackStrategyFailure', this accepts
--   any 'Exception' type as an argument.
toStrategyFailure :: Exception e => String -> e -> StrategyFailure
toStrategyFailure s = stackStrategyFailure s . toException
{-# INLINE toStrategyFailure #-}

-- | Show a 'StrategyFailure' in a human-friendly way.
displayStrategyFailure :: StrategyFailure -> String
displayStrategyFailure (StrategyFailure s Nothing) =
    s ++ " strategy failed."
displayStrategyFailure (StrategyFailure s (Just e)) =
    s ++ " strategy failed, because " ++ showKureExc e

-- | A conditional test (other than a 'NodeMismatch') failure exception type,
--   containing an explanation of what failed and why.
data ConditionalFailure = ConditionalFailure String
  deriving (Eq, Show, Typeable)

instance Exception ConditionalFailure where
#if __GLASGOW_HASKELL__ >= 710
    displayException = displayConditionalFailure
    {-# INLINE displayException #-}
#endif

-- | Construct a 'ConditionalFailure' from an explanation as for what failed and why.
conditionalFailure :: String -> ConditionalFailure
conditionalFailure = ConditionalFailure
{-# INLINE conditionalFailure #-}

-- | Show a 'ConditionalFailure' in a human-friendly way.
displayConditionalFailure :: ConditionalFailure -> String
displayConditionalFailure (ConditionalFailure why) = why
{-# INLINE displayConditionalFailure #-}

-- | If 'SomeException' contains a KURE-related 'Exception', show it in
--   a human-friendly way.
showKureExc :: SomeException -> String
#if __GLASGOW_HASKELL__ >= 710
showKureExc = displayException
#else
showKureExc e =
  case fromException e of
    Just nm@NodeMismatch{} -> displayNodeMismatch nm
    Nothing                -> case fromException e of
      Just sf@StrategyFailure{} -> displayStrategyFailure sf
      Nothing                   -> case fromException e of
        Just cf@ConditionalFailure{} -> displayConditionalFailure cf
        Nothing                      -> show e
#endif
