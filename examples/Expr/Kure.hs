{-# LANGUAGE MultiParamTypeClasses #-}

module Expr.Kure where

import Control.Monad

import Language.KURE

import Expr.AST

---------------------------------------------------------------------------

data Context = Context AbsolutePath [(Name,Expr)] -- A list of bindings.
                                                  -- We assume no shadowing in the language.

instance PathContext Context where
-- absPath :: Context -> AbsolutePath
   absPath (Context p _) = p

-- (@@) :: Context -> Int -> Context
   (Context p defs) @@ n = Context (p @@ n) defs

addDef :: Name -> Expr -> Context -> Context
addDef v e (Context p defs) = Context p ((v,e):defs)

updateContextCmd :: Cmd -> Context -> Context
updateContextCmd (Seq c1 c2)  = updateContextCmd c2 . updateContextCmd c1
updateContextCmd (Assign v e) = (addDef v e)

initialContext :: Context
initialContext = Context rootAbsPath []

lookupDef :: Monad m => Name -> Context -> m Expr
lookupDef v (Context _ defs) = maybe (fail $ v ++ " not found in context") return (lookup v defs)

---------------------------------------------------------------------------

data Generic = GExpr Expr
             | GCmd Cmd

---------------------------------------------------------------------------

instance Injection Expr Generic where
  inject = GExpr

  project (GExpr e) = Just e
  project _         = Nothing

instance Injection Cmd Generic where
  inject = GCmd

  project (GCmd c) = Just c
  project _        = Nothing

---------------------------------------------------------------------------

instance Walker Context Generic where
-- allR :: MonadCatch m => Rewrite Context m Generic -> Rewrite Context m Generic
   allR r = prefixFailMsg "allR failed: " $
            rewrite $ \ c g -> case g of
              GExpr e  -> inject <$> apply allRexpr c e
              GCmd cm  -> inject <$> apply allRcmd c cm
     where
       allRexpr = readerT $ \ expr -> case expr of
                    Add _ _  -> addAllR (extractR r) (extractR r)
                    ESeq _ _ -> eseqAllR (extractR r) (extractR r)
                    _        -> idR

       allRcmd  = readerT $ \ cmd -> case cmd of
                    Seq _ _    -> seqAllR (extractR r) (extractR r)
                    Assign _ _ -> assignR (extractR r)

---------------------------------------------------------------------------

seqT :: Monad m => Translate Context m Cmd a1 -> Translate Context m Cmd a2 -> (a1 -> a2 -> b) -> Translate Context m Cmd b
seqT t1 t2 f = translate $ \ c cm -> case cm of
                                       Seq cm1 cm2 -> f <$> apply t1 (c @@ 0) cm1 <*> apply t2 (updateContextCmd cm1 c @@ 1) cm2
                                       _           -> fail "not a Seq"

seqAllR :: Monad m => Rewrite Context m Cmd -> Rewrite Context m Cmd -> Rewrite Context m Cmd
seqAllR r1 r2 = seqT r1 r2 Seq

seqAnyR :: MonadCatch m => Rewrite Context m Cmd -> Rewrite Context m Cmd -> Rewrite Context m Cmd
seqAnyR r1 r2 = unwrapAnyR $ seqAllR (wrapAnyR r1) (wrapAnyR r2)

seqOneR :: MonadCatch m => Rewrite Context m Cmd -> Rewrite Context m Cmd -> Rewrite Context m Cmd
seqOneR r1 r2 = unwrapOneR $ seqAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------

assignT :: Monad m => Translate Context m Expr a -> (Name -> a -> b) -> Translate Context m Cmd b
assignT t f = translate $ \ c cm -> case cm of
                                      Assign n e -> f n <$> apply t (c @@ 0) e
                                      _          -> fail "not an Assign"

assignR :: Monad m => Rewrite Context m Expr -> Rewrite Context m Cmd
assignR r = assignT r Assign

---------------------------------------------------------------------------

varT :: Monad m => (Name -> b) -> Translate Context m Expr b
varT f = contextfreeT $ \ e -> case e of
                                 Var v -> return (f v)
                                 _     -> fail "not a Var"

---------------------------------------------------------------------------

litT :: Monad m => (Int -> b) -> Translate Context m Expr b
litT f = contextfreeT $ \ e -> case e of
                                 Lit v -> return (f v)
                                 _     -> fail "not a Lit"

---------------------------------------------------------------------------

addT :: Monad m => Translate Context m Expr a1 -> Translate Context m Expr a2 -> (a1 -> a2 -> b) -> Translate Context m Expr b
addT t1 t2 f = translate $ \ c e -> case e of
                                      Add e1 e2 -> f <$> apply t1 (c @@ 0) e1 <*> apply t2 (c @@ 1) e2
                                      _         -> fail "not an Add"

addAllR :: Monad m => Rewrite Context m Expr -> Rewrite Context m Expr -> Rewrite Context m Expr
addAllR r1 r2 = addT r1 r2 Add

addAnyR :: MonadCatch m => Rewrite Context m Expr -> Rewrite Context m Expr -> Rewrite Context m Expr
addAnyR r1 r2 = unwrapAnyR $ addAllR (wrapAnyR r1) (wrapAnyR r2)

addOneR :: MonadCatch m => Rewrite Context m Expr -> Rewrite Context m Expr -> Rewrite Context m Expr
addOneR r1 r2 = unwrapOneR $ addAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------

eseqT :: Monad m => Translate Context m Cmd a1 -> Translate Context m Expr a2 -> (a1 -> a2 -> b) -> Translate Context m Expr b
eseqT t1 t2 f = translate $ \ c e -> case e of
                                       ESeq cm e1 -> f <$> apply t1 (c @@ 0) cm <*> apply t2 (updateContextCmd cm c @@ 1) e1
                                       _          -> fail "not an ESeq"

eseqAllR :: Monad m => Rewrite Context m Cmd -> Rewrite Context m Expr -> Rewrite Context m Expr
eseqAllR r1 r2 = eseqT r1 r2 ESeq

eseqAnyR :: MonadCatch m => Rewrite Context m Cmd -> Rewrite Context m Expr -> Rewrite Context m Expr
eseqAnyR r1 r2 = unwrapAnyR $ eseqAllR (wrapAnyR r1) (wrapAnyR r2)

eseqOneR :: MonadCatch m => Rewrite Context m Cmd -> Rewrite Context m Expr -> Rewrite Context m Expr
eseqOneR r1 r2 = unwrapOneR $ eseqAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------

-- I find it annoying that Applicative is not a superclass of Monad.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}

---------------------------------------------------------------------------
