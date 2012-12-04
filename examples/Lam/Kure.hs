{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Lam.Kure where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Category
import Control.Monad

import Language.KURE
import Language.KURE.Utilities

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

newtype LamM a = LamM {lamM :: Int -> (Int, Either String a)}

runLamM :: LamM a -> Either String a
runLamM m = snd (lamM m 0)

instance Monad LamM where
  return a = LamM (\n -> (n,Right a))
  (LamM f) >>= gg = LamM $ \ n -> case f n of
                                    (n', Left msg) -> (n', Left msg)
                                    (n', Right a)  -> lamM (gg a) n'
  fail msg = LamM (\ n -> (n, Left msg))

instance MonadCatch LamM where
  (LamM f) `catchM` g = LamM $ \ n -> case f n of
                                        (n', Left msg) -> lamM (g msg) n'
                                        (n', Right a)  -> (n', Right a)

instance Functor LamM where
  fmap = liftM

instance Applicative LamM where
  pure  = return
  (<*>) = ap

-------------------------------------------------------------------------------

suggestName :: LamM Name
suggestName = LamM (\n -> ((n+1), Right (show n)))

freshName :: [Name] -> LamM Name
freshName vs = do v <- suggestName
                  if v `elem` vs
                    then freshName vs
                    else return v

-------------------------------------------------------------------------------

type TranslateExp b = Translate Context LamM Exp b
type RewriteExp     = TranslateExp Exp

applyExp :: TranslateExp b -> Exp -> Either String b
applyExp f = runLamM . apply f initialContext

-------------------------------------------------------------------------------

instance Node Exp where

   numChildren (Var _)   = 0
   numChildren (Lam _ _) = 1
   numChildren (App _ _) = 2

instance Walker Context LamM Exp where
   childL n = lens $
     case n of
       0 ->    appT exposeT id (childL0of2 App)
            <+ lamT exposeT (childL1of2 Lam)

       1 -> appT id exposeT (childL1of2 App)

       _ -> fail (missingChild n)

-------------------------------------------------------------------------------

-- | Congruence combinators.
--   Using these ensures that the context is updated consistantly.

varT :: (Name -> b) -> TranslateExp b
varT f = contextfreeT $ \ e -> case e of
                                 Var n -> return (f n)
                                 _     -> fail "no match for Var"

-------------------------------------------------------------------------------

lamT :: TranslateExp a -> (Name -> a -> b) -> TranslateExp b
lamT t f = translate $ \ c e -> case e of
                                  Lam v e1 -> f v <$> apply t (addBinding v c @@ 0) e1
                                  _        -> fail "no match for Lam"

lamR :: RewriteExp -> RewriteExp
lamR r = lamT r Lam

-------------------------------------------------------------------------------

appT' :: TranslateExp a1 -> TranslateExp a2 -> (LamM a1 -> LamM a2 -> LamM b) -> TranslateExp b
appT' t1 t2 f = translate $ \ c e -> case e of
         App e1 e2 -> f (apply t1 (c @@ 0) e1) (apply t2 (c @@ 1) e2)
         _         -> fail "no match for App"

appT :: TranslateExp a1 -> TranslateExp a2 -> (a1 -> a2 -> b) -> TranslateExp b
appT t1 t2 f = appT' t1 t2 (liftA2 f)

appAllR :: RewriteExp -> RewriteExp -> RewriteExp
appAllR r1 r2 = appT r1 r2 App

appAnyR :: RewriteExp -> RewriteExp -> RewriteExp
appAnyR r1 r2 = appT' (attemptR r1) (attemptR r2) (attemptAny2 App)

appOneR :: RewriteExp -> RewriteExp -> RewriteExp
appOneR r1 r2 = appT' (withArgumentT r1) (withArgumentT r2) (attemptOne2 App)

-------------------------------------------------------------------------------
