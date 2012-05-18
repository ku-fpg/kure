{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module ExpKure where

import Data.Monoid
import Control.Applicative

import Language.KURE
import Language.KURE.Utilities
import Exp

-------------------------------------------------------------------------------

type TranslateExp b = Translate Context ExpM Exp b
type RewriteExp     = TranslateExp Exp

applyExp :: TranslateExp b -> Exp -> Either String b
applyExp f = runExpM . apply f []

-------------------------------------------------------------------------------

instance Term Exp where
   type Generic Exp = Exp  -- Exp is its own Generic root.

instance WalkerR Context ExpM Exp where
   allR r =    varT Var
            <+ lamT r Lam
            <+ appT r r App

   anyR r =    lamT r Lam
            <+ appT' (attemptR r) (attemptR r) (attemptAny2 App)

instance Monoid b => WalkerT Context ExpM Exp b where
   crushT t =    varT (const mempty)
              <+ lamT t (\ _ -> id)
              <+ appT t t mappend

instance WalkerL Context ExpM Exp where
   chooseL 0 =    appT exposeT idR (\ cx e2 -> (cx, \ e1 -> pure $ App e1 e2) )
               <+ lamT exposeT     (\ v cx  -> (cx, \ e1 -> pure $ Lam v e1 ) )

   chooseL 1 =    appT idR exposeT (\ e1 cx -> (cx, \ e2 -> pure $ App e1 e2) )

   chooseL n =    missingChildL n

-------------------------------------------------------------------------------

-- | Scoping combinators.
--   Using these ensures that the context is updated consistantly.

varT :: (Name -> b) -> TranslateExp b
varT f = liftMT $ \ e -> case e of
        Var n -> pure (f n)
        _     -> fail "no match for Var"

lamT :: TranslateExp a -> (Name -> a -> b) -> TranslateExp b
lamT t f = translate $ \ c e -> case e of
                                  Lam v e1 -> f v <$> apply t (v:c) e1
                                  _        -> fail "no match for Lam"

appT' :: TranslateExp a1 -> TranslateExp a2 -> (ExpM a1 -> ExpM a2 -> ExpM b) -> TranslateExp b
appT' t1 t2 f = translate $ \ c e -> case e of
         App e1 e2 -> f (apply t1 c e1) (apply t2 c e2)
         _         -> fail "no match for App"

appT :: TranslateExp a1 -> TranslateExp a2 -> (a1 -> a2 -> b) -> TranslateExp b
appT t1 t2 f = appT' t1 t2 (liftA2 f)


-------------------------------------------------------------------------------
