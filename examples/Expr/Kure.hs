{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Expr.Kure where

import Prelude hiding (id , (.))

import Control.Category
import Control.Monad

import Language.KURE
import Language.KURE.Injection
import Language.KURE.Utilities

import Expr.AST

---------------------------------------------------------------------------

data Context = Context AbsolutePath [(Name,Expr)] -- A list of bindings.
                                                  -- We assume no shadowing in the language.

instance PathContext Context where
  contextPath (Context p _) = p

addDef :: Name -> Expr -> Context -> Context
addDef v e (Context p defs) = Context p ((v,e):defs)

updateContextCmd :: Cmd -> Context -> Context
updateContextCmd (Seq c1 c2)  = updateContextCmd c2 . updateContextCmd c1
updateContextCmd (Assign v e) = (addDef v e)

(@@) :: Context -> Int -> Context
(Context p defs) @@ n = Context (extendAbsPath n p) defs

initialContext :: Context
initialContext = Context rootAbsPath []

lookupDef :: Monad m => Name -> Context -> m Expr
lookupDef v (Context _ defs) = maybe (fail $ v ++ " not found in context") return $ lookup v defs

---------------------------------------------------------------------------

data GenericExpr = GExpr Expr
                 | GCmd Cmd

---------------------------------------------------------------------------

instance Injection Expr GenericExpr where
  inject = GExpr

  retract (GExpr e) = Just e
  retract _         = Nothing

instance Injection Cmd GenericExpr where
  inject = GCmd

  retract (GCmd c) = Just c
  retract _        = Nothing

---------------------------------------------------------------------------

-- I find it annoying that Applicative is not a superclass of Monad.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap

instance Node Context GenericExpr where

  childrenL = multiLens $ translate $ \ c g -> case g of
                GExpr e  -> injectLensReturn $ apply childrenLexpr c e
                GCmd cm  -> injectLensReturn $ apply childrenLcmd c cm
    where
      childrenLexpr = readerT $ \ expr ->
                         case expr of
                           Add _ _  -> addT  exposeInjectT exposeInjectT (\ ce1 ce2 -> ([ce1,ce2], \ [e1,e2] -> Add  <$> retractM e1 <*> retractM e2))
                           ESeq _ _ -> eseqT exposeInjectT exposeInjectT (\ ccm ce  -> ([ccm,ce],  \ [cm,e]  -> ESeq <$> retractM cm <*> retractM e))
                           Var _    -> varT                              (\ v       -> ([],        \ []      -> return (Var v)))
                           Lit _    -> litT                              (\ n       -> ([],        \ []      -> return (Lit n)))

      -- I believe the above definition is more efficient than the one below, but if not then I'll revert to using (<+).

      -- childrenLexpr =    addT  exposeInjectT exposeInjectT (\ ce1 ce2 -> ([ce1,ce2], \ [e1,e2] -> Add  <$> retractM e1 <*> retractM e2))
      --                 <+ eseqT exposeInjectT exposeInjectT (\ ccm ce  -> ([ccm,ce],  \ [cm,e]  -> ESeq <$> retractM cm <*> retractM e))
      --                 <+ litT                              (\ n       -> ([],        \ []      -> return (Lit n)))
      --                 <+ varT                              (\ v       -> ([],        \ []      -> return (Var v)))

      childrenLcmd = readerT $ \ cmd ->
                        case cmd of
                          Seq _ _    -> seqT    exposeInjectT exposeInjectT (\ ccm1 ccm2 -> ([ccm1,ccm2], \ [cm1,cm2] -> Seq <$> retractM cm1 <*> retractM cm2))
                          Assign _ _ -> assignT exposeInjectT               (\ v ce      -> ([ce],        \ [e]       -> Assign v <$> retractM e))

---------------------------------------------------------------------------

seqT' :: Monad m => Translate Context m Cmd a1 -> Translate Context m Cmd a2 -> (m a1 -> m a2 -> m b) -> Translate Context m Cmd b
seqT' t1 t2 f = translate $ \ c cm -> case cm of
                                       Seq cm1 cm2 -> f (apply t1 (c @@ 0) cm1) (apply t2 (updateContextCmd cm1 c @@ 1) cm2)
                                       _           -> fail "not a Seq"

seqT :: Monad m => Translate Context m Cmd a1 -> Translate Context m Cmd a2 -> (a1 -> a2 -> b) -> Translate Context m Cmd b
seqT t1 t2 f = seqT' t1 t2 (liftM2 f)

seqAllR :: Monad m => Rewrite Context m Cmd -> Rewrite Context m Cmd -> Rewrite Context m Cmd
seqAllR r1 r2 = seqT r1 r2 Seq

seqAnyR :: MonadCatch m => Rewrite Context m Cmd -> Rewrite Context m Cmd -> Rewrite Context m Cmd
seqAnyR r1 r2 = seqT' (attemptR r1) (attemptR r2) (anyR_helper2 Seq)

seqOneR :: MonadCatch m => Rewrite Context m Cmd -> Rewrite Context m Cmd -> Rewrite Context m Cmd
seqOneR r1 r2 = seqT' (withArgumentT r1) (withArgumentT r2) (oneR_helper2 Seq)

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

addT' :: Monad m => Translate Context m Expr a1 -> Translate Context m Expr a2 -> (m a1 -> m a2 -> m b) -> Translate Context m Expr b
addT' t1 t2 f = translate $ \ c e -> case e of
                                       Add e1 e2 -> f (apply t1 (c @@ 0) e1) (apply t2 (c @@ 1) e2)
                                       _         -> fail "not an Add"

addT :: Monad m => Translate Context m Expr a1 -> Translate Context m Expr a2 -> (a1 -> a2 -> b) -> Translate Context m Expr b
addT t1 t2 f = addT' t1 t2 (liftM2 f)

addAllR :: Monad m => Rewrite Context m Expr -> Rewrite Context m Expr -> Rewrite Context m Expr
addAllR r1 r2 = addT r1 r2 Add

addAnyR :: MonadCatch m => Rewrite Context m Expr -> Rewrite Context m Expr -> Rewrite Context m Expr
addAnyR r1 r2 = addT' (attemptR r1) (attemptR r2) (anyR_helper2 Add)

addOneR :: MonadCatch m => Rewrite Context m Expr -> Rewrite Context m Expr -> Rewrite Context m Expr
addOneR r1 r2 = addT' (withArgumentT r1) (withArgumentT r2) (oneR_helper2 Add)

---------------------------------------------------------------------------

eseqT' :: Monad m => Translate Context m Cmd a1 -> Translate Context m Expr a2 -> (m a1 -> m a2 -> m b) -> Translate Context m Expr b
eseqT' t1 t2 f = translate $ \ c e -> case e of
                                        ESeq cm e1 -> f (apply t1 (c @@ 0) cm) (apply t2 (updateContextCmd cm c @@ 1) e1)
                                        _          -> fail "not an ESeq"

eseqT :: Monad m => Translate Context m Cmd a1 -> Translate Context m Expr a2 -> (a1 -> a2 -> b) -> Translate Context m Expr b
eseqT t1 t2 f = eseqT' t1 t2 (liftM2 f)

eseqAllR :: Monad m => Rewrite Context m Cmd -> Rewrite Context m Expr -> Rewrite Context m Expr
eseqAllR r1 r2 = eseqT r1 r2 ESeq

eseqAnyR :: MonadCatch m => Rewrite Context m Cmd -> Rewrite Context m Expr -> Rewrite Context m Expr
eseqAnyR r1 r2 = eseqT' (attemptR r1) (attemptR r2) (anyR_helper2 ESeq)

eseqOneR :: MonadCatch m => Rewrite Context m Cmd -> Rewrite Context m Expr -> Rewrite Context m Expr
eseqOneR r1 r2 = eseqT' (withArgumentT r1) (withArgumentT r2) (oneR_helper2 ESeq)

---------------------------------------------------------------------------
