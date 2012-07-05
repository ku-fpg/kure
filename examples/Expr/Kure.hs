{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Expr.Kure where

import Prelude hiding (id , (.))

import Control.Category
import Control.Applicative

import Data.Monoid

import Language.KURE
import Language.KURE.Injection
import Language.KURE.Utilities

import Expr.AST

-- NOTE: allT, allR and anyR have been defined to serve as examples,
--       but using the default instances would be fine (just slightly less efficient).

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

lookupDef :: Name -> Context -> KureMonad Expr
lookupDef v (Context _ defs) = maybe (fail $ v ++ " not found in context") return $ lookup v defs

---------------------------------------------------------------------------

type TranslateE a b = Translate Context KureMonad a b
type RewriteE a = TranslateE a a

applyE :: TranslateE a b -> a -> Either String b
applyE t = runKureMonad Right Left . apply t initialContext

---------------------------------------------------------------------------

data GenericExpr = GExpr Expr
                 | GCmd Cmd

instance Node GenericExpr where
  type Generic GenericExpr = GenericExpr

  numChildren (GExpr e) = numChildren e
  numChildren (GCmd c)  = numChildren c

---------------------------------------------------------------------------

instance Walker Context KureMonad GenericExpr where

  childL n = lens $ translate $ \ c g -> case g of
                                           GExpr e -> childLgeneric n c e
                                           GCmd cm -> childLgeneric n c cm

  allT t = translate $ \ c g -> case g of
                                  GExpr e -> allTgeneric t c e
                                  GCmd cm -> allTgeneric t c cm

  oneT t = translate $ \ c g -> case g of
                                  GExpr e -> oneTgeneric t c e
                                  GCmd cm -> oneTgeneric t c cm

  allR r = rewrite $ \ c g -> case g of
                                GExpr e -> allRgeneric r c e
                                GCmd cm -> allRgeneric r c cm

  anyR r = rewrite $ \ c g -> case g of
                                GExpr e -> anyRgeneric r c e
                                GCmd cm -> anyRgeneric r c cm

  oneR r = rewrite $ \ c g -> case g of
                                GExpr e -> oneRgeneric r c e
                                GCmd cm -> oneRgeneric r c cm

---------------------------------------------------------------------------

instance Injection Expr GenericExpr where
  inject = GExpr

  retract (GExpr e) = Just e
  retract _         = Nothing

instance Node Expr where
  type Generic Expr = GenericExpr

  numChildren (Add _ _)  = 2
  numChildren (ESeq _ _) = 2
  numChildren (Var _)    = 0
  numChildren (Lit _)    = 0

instance Walker Context KureMonad Expr where
  childL n = lens $
    case n of
      0 ->    addT  exposeT id (childL0of2 Add)
           <+ eseqT exposeT id (childL0of2 ESeq)
      1 ->    addT  id exposeT (childL1of2 Add)
           <+ eseqT id exposeT (childL1of2 ESeq)
      _ -> fail (missingChild n)

  allT t =  varT (\ _ -> mempty)
         <+ litT (\ _ -> mempty)
         <+ addT (extractT t) (extractT t) mappend
         <+ eseqT (extractT t) (extractT t) mappend

  oneT t =  addT' (extractT t) (extractT t) (<<+)
         <+ eseqT' (extractT t) (extractT t) (<<+)
         <+ fail "oneT failed"

  allR r =  varT Var
         <+ litT Lit
         <+ addAllR (extractR r) (extractR r)
         <+ eseqAllR (extractR r) (extractR r)

  anyR r =  addAnyR (extractR r) (extractR r)
         <+ eseqAnyR (extractR r) (extractR r)
         <+ fail "anyR failed"

  oneR r =  addOneR (extractR r) (extractR r)
         <+ eseqOneR (extractR r) (extractR r)
         <+ fail "oneR failed"

---------------------------------------------------------------------------

instance Injection Cmd GenericExpr where
  inject = GCmd

  retract (GCmd c) = Just c
  retract _        = Nothing

instance Node Cmd where
  type Generic Cmd = GenericExpr

  numChildren (Seq _ _)    = 2
  numChildren (Assign _ _) = 1

instance Walker Context KureMonad Cmd where
  childL n = lens $
    case n of
      0 ->    seqT exposeT id (childL0of2 Seq)
           <+ assignT exposeT (childL1of2 Assign)
      1 ->    seqT id exposeT (childL1of2 Seq)
           <+ fail (missingChild n)
      _ -> fail (missingChild n)

  allT t =  seqT (extractT t) (extractT t) mappend
         <+ assignT (extractT t) (\ _ -> id)

  oneT t =  seqT' (extractT t) (extractT t) (<<+)
         <+ assignT (extractT t) (\ _ -> id)
         <+ fail "oneT failed"

  allR r =  seqAllR (extractR r) (extractR r)
         <+ assignR (extractR r)

  anyR r =  seqAnyR (extractR r) (extractR r)
         <+ assignR (extractR r)
         <+ fail "anyR failed"

  oneR r =  seqOneR (extractR r) (extractR r)
         <+ assignR (extractR r)
         <+ fail "oneR failed"

---------------------------------------------------------------------------

seqT' :: TranslateE Cmd a1 -> TranslateE Cmd a2 -> (KureMonad a1 -> KureMonad a2 -> KureMonad b) -> TranslateE Cmd b
seqT' t1 t2 f = translate $ \ c cm -> case cm of
                                       Seq cm1 cm2 -> f (apply t1 (c @@ 0) cm1) (apply t2 (updateContextCmd cm1 c @@ 1) cm2)
                                       _           -> fail "not a Seq"

seqT :: TranslateE Cmd a1 -> TranslateE Cmd a2 -> (a1 -> a2 -> b) -> TranslateE Cmd b
seqT t1 t2 f = seqT' t1 t2 (liftA2 f)

seqAllR :: RewriteE Cmd -> RewriteE Cmd -> RewriteE Cmd
seqAllR r1 r2 = seqT r1 r2 Seq

seqAnyR :: RewriteE Cmd -> RewriteE Cmd -> RewriteE Cmd
seqAnyR r1 r2 = seqT' (attemptR r1) (attemptR r2) (attemptAny2 Seq)

seqOneR :: RewriteE Cmd -> RewriteE Cmd -> RewriteE Cmd
seqOneR r1 r2 = seqT' (withArgumentT r1) (withArgumentT r2) (attemptOne2 Seq)

---------------------------------------------------------------------------

assignT :: TranslateE Expr a -> (Name -> a -> b) -> TranslateE Cmd b
assignT t f = translate $ \ c cm -> case cm of
                                      Assign n e -> f n <$> apply t (c @@ 0) e
                                      _          -> fail "not an Assign"

assignR :: RewriteE Expr -> RewriteE Cmd
assignR r = assignT r Assign

---------------------------------------------------------------------------

varT :: (Name -> b) -> TranslateE Expr b
varT f = contextfreeT $ \ e -> case e of
                                 Var v -> pure (f v)
                                 _     -> fail "not a Var"

---------------------------------------------------------------------------

litT :: (Int -> b) -> TranslateE Expr b
litT f = contextfreeT $ \ e -> case e of
                                 Lit v -> pure (f v)
                                 _     -> fail "not a Lit"

---------------------------------------------------------------------------

addT' :: TranslateE Expr a1 -> TranslateE Expr a2 -> (KureMonad a1 -> KureMonad a2 -> KureMonad b) -> TranslateE Expr b
addT' t1 t2 f = translate $ \ c e -> case e of
                                       Add e1 e2 -> f (apply t1 (c @@ 0) e1) (apply t2 (c @@ 1) e2)
                                       _         -> fail "not an Add"

addT :: TranslateE Expr a1 -> TranslateE Expr a2 -> (a1 -> a2 -> b) -> TranslateE Expr b
addT t1 t2 f = addT' t1 t2 (liftA2 f)

addAllR :: RewriteE Expr -> RewriteE Expr -> RewriteE Expr
addAllR r1 r2 = addT r1 r2 Add

addAnyR :: RewriteE Expr -> RewriteE Expr -> RewriteE Expr
addAnyR r1 r2 = addT' (attemptR r1) (attemptR r2) (attemptAny2 Add)

addOneR :: RewriteE Expr -> RewriteE Expr -> RewriteE Expr
addOneR r1 r2 = addT' (withArgumentT r1) (withArgumentT r2) (attemptOne2 Add)

---------------------------------------------------------------------------

eseqT' :: TranslateE Cmd a1 -> TranslateE Expr a2 -> (KureMonad a1 -> KureMonad a2 -> KureMonad b) -> TranslateE Expr b
eseqT' t1 t2 f = translate $ \ c e -> case e of
                                        ESeq cm e1 -> f (apply t1 (c @@ 0) cm) (apply t2 (updateContextCmd cm c @@ 1) e1)
                                        _          -> fail "not an ESeq"

eseqT :: TranslateE Cmd a1 -> TranslateE Expr a2 -> (a1 -> a2 -> b) -> TranslateE Expr b
eseqT t1 t2 f = eseqT' t1 t2 (liftA2 f)

eseqAllR :: RewriteE Cmd -> RewriteE Expr -> RewriteE Expr
eseqAllR r1 r2 = eseqT r1 r2 ESeq

eseqAnyR :: RewriteE Cmd -> RewriteE Expr -> RewriteE Expr
eseqAnyR r1 r2 = eseqT' (attemptR r1) (attemptR r2) (attemptAny2 ESeq)

eseqOneR :: RewriteE Cmd -> RewriteE Expr -> RewriteE Expr
eseqOneR r1 r2 = eseqT' (withArgumentT r1) (withArgumentT r2) (attemptOne2 ESeq)

---------------------------------------------------------------------------
