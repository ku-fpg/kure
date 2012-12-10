{-# LANGUAGE MultiParamTypeClasses #-}

module Lam.Kure where

import Control.Monad

import Language.KURE

import Lam.AST

-------------------------------------------------------------------------------

data Context = Context AbsolutePath [Name] -- bound variable names

instance PathContext Context where
  contextPath (Context p _) = p

addBinding :: Name -> Context -> Context
addBinding v (Context p vs) = Context p (v:vs)

(@@) :: Context -> Int -> Context
(Context p vs) @@ n = Context (extendAbsPath n p) vs

initialContext :: Context
initialContext = Context rootAbsPath []

bindings :: Context -> [Name]
bindings (Context _ vs) = vs

boundIn :: Name -> Context -> Bool
boundIn v c = v `elem` bindings c

freeIn :: Name -> Context -> Bool
freeIn v c = not (v `boundIn` c)

-------------------------------------------------------------------------------

type TranslateExp m b = Translate Context m Exp b
type RewriteExp m     = TranslateExp m Exp

-------------------------------------------------------------------------------

instance Walker Context Exp where
-- allR :: MonadCatch m => RewriteExp m -> RewriteExp m
   allR r = prefixFailMsg "allR failed: " $
            readerT $ \ e -> case e of
              App _ _ -> appAllR r r
              Lam _ _ -> lamR r
              _       -> idR

-------------------------------------------------------------------------------

-- | Congruence combinators.
--   Using these ensures that the context is updated consistantly.

varT :: Monad m => (Name -> b) -> TranslateExp m b
varT f = contextfreeT $ \ e -> case e of
                                 Var n -> return (f n)
                                 _     -> fail "no match for Var"

-------------------------------------------------------------------------------

lamT :: Monad m => TranslateExp m a -> (Name -> a -> b) -> TranslateExp m b
lamT t f = translate $ \ c e -> case e of
                                  Lam v e1 -> f v <$> apply t (addBinding v c @@ 0) e1
                                  _        -> fail "no match for Lam"

lamR :: Monad m => RewriteExp m -> RewriteExp m
lamR r = lamT r Lam

-------------------------------------------------------------------------------

appT :: Monad m => TranslateExp m a1 -> TranslateExp m a2 -> (a1 -> a2 -> b) -> TranslateExp m b
appT t1 t2 f = translate $ \ c e -> case e of
                                      App e1 e2 -> f <$> apply t1 (c @@ 0) e1 <*> apply t2 (c @@ 1) e2
                                      _         -> fail "no match for App"

appAllR :: Monad m => RewriteExp m -> RewriteExp m -> RewriteExp m
appAllR r1 r2 = appT r1 r2 App

appAnyR :: MonadCatch m => RewriteExp m -> RewriteExp m -> RewriteExp m
appAnyR r1 r2 = unwrapAnyR $ appAllR (wrapAnyR r1) (wrapAnyR r2)

appOneR :: MonadCatch m => RewriteExp m -> RewriteExp m -> RewriteExp m
appOneR r1 r2 = unwrapOneR $ appAllR (wrapOneR r1) (wrapOneR r2)

-------------------------------------------------------------------------------

-- I find it annoying that Applicative is not a superclass of Monad.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}

-------------------------------------------------------------------------------
