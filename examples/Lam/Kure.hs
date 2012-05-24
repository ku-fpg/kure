{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Lam.Kure where

import Control.Applicative

import Language.KURE
import Language.KURE.Utilities

import Lam.AST

-------------------------------------------------------------------------------

type TranslateExp b = Translate Context LamM Exp b
type RewriteExp     = TranslateExp Exp

applyExp :: TranslateExp b -> Exp -> Either String b
applyExp f = runLamM . apply f []

-------------------------------------------------------------------------------

instance Term Exp where
   type Generic Exp = Exp  -- Exp is its own Generic

   numChildren (Var _)   = 0
   numChildren (Lam _ _) = 1
   numChildren (App _ _) = 2

instance Walker Context LamM Exp where
   childL n = case n of
                0 ->    appT exposeT idR (childL0of2 App)
                     <+ lamT exposeT (childL1of2 Lam)

                1 ->    appT idR exposeT (childL1of2 App)

                _ -> missingChildL n

-------------------------------------------------------------------------------

-- | Congruence combinators.
--   Using these ensures that the context is updated consistantly.

varT :: (Name -> b) -> TranslateExp b
varT f = liftMT $ \ e -> case e of
        Var n -> pure (f n)
        _     -> fail "no match for Var"

-------------------------------------------------------------------------------

lamT :: TranslateExp a -> (Name -> a -> b) -> TranslateExp b
lamT t f = translate $ \ c e -> case e of
                                  Lam v e1 -> f v <$> apply t (v:c) e1
                                  _        -> fail "no match for Lam"

lamR :: RewriteExp -> RewriteExp
lamR r = lamT r Lam

-------------------------------------------------------------------------------

appT' :: TranslateExp a1 -> TranslateExp a2 -> (LamM a1 -> LamM a2 -> LamM b) -> TranslateExp b
appT' t1 t2 f = translate $ \ c e -> case e of
         App e1 e2 -> f (apply t1 c e1) (apply t2 c e2)
         _         -> fail "no match for App"

appT :: TranslateExp a1 -> TranslateExp a2 -> (a1 -> a2 -> b) -> TranslateExp b
appT t1 t2 f = appT' t1 t2 (liftA2 f)

appR :: RewriteExp -> RewriteExp -> RewriteExp
appR r1 r2 = appT' (attemptR r1) (attemptR r2) (attemptAny2 App)

-------------------------------------------------------------------------------
