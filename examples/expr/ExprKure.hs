{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module ExprKure where

import Control.Applicative
import Data.Monoid

import Language.KURE

import Expr

---------------------------------------------------------------------------

type TranslateE a b = Translate Context Maybe a b
type RewriteE a = TranslateE a a

---------------------------------------------------------------------------

data GenericExpr = GExpr Expr
                 | GCmd Cmd

instance Term GenericExpr where
  type Generic GenericExpr = GenericExpr

---------------------------------------------------------------------------

instance WalkerR Context Maybe GenericExpr where
  allR r = rewrite $ \ c g -> case g of
                                GExpr e -> allRgeneric r c e
                                GCmd cm -> allRgeneric r c cm

  anyR r = rewrite $ \ c g -> case g of
                                GExpr e -> anyRgeneric r c e
                                GCmd cm -> anyRgeneric r c cm


instance Monoid b => WalkerT Context Maybe GenericExpr b where
  crushT t = translate $ \ c g -> case g of
                                    GExpr e -> crushTgeneric t c e
                                    GCmd cm -> crushTgeneric t c cm

instance WalkerL Context Maybe GenericExpr where
  chooseL n = lens $ \ c g -> case g of
                                GExpr e -> chooseLgeneric n c e
                                GCmd cm -> chooseLgeneric n c cm

---------------------------------------------------------------------------

instance Injection Expr GenericExpr where
  inject = GExpr

  retract (GExpr e) = Just e
  retract _         = Nothing

instance Term Expr where
  type Generic Expr = GenericExpr

instance WalkerR Context Maybe Expr where
  allR r =  varT Var
         <+ litT Lit
         <+ addT (extractR r) (extractR r) Add
         <+ eseqT (extractR r) (extractR r) ESeq

  anyR r =  addT' (attemptR $ extractR r) (attemptR $ extractR r) (attemptAny2 Add)
         <+ eseqT' (attemptR $ extractR r) (attemptR $ extractR r) (attemptAny2 ESeq)
         <+ fail "anyR failed"

instance Monoid b => WalkerT Context Maybe Expr b where
  crushT t =  varT (const mempty)
           <+ litT (const mempty)
           <+ addT (extractT t) (extractT t) mappend

instance WalkerL Context Maybe Expr where
  chooseL 0 =  (addT  exposeT idR ( \ ce1 e2 -> (ce1, \ e1 -> pure (Add e1 e2)) ) `composeL` promoteL)
            <+ (eseqT exposeT idR ( \ ccm e  -> (ccm, \ cm -> pure (ESeq cm e)) ) `composeL` promoteL)
            <+ missingChildL 0

  chooseL 1 =  (addT  idR exposeT ( \ e1 ce2 -> (ce2, \ e2 -> pure (Add e1 e2)) ) `composeL` promoteL)
            <+ (eseqT idR exposeT ( \ cm ce  -> (ce,  \ e  -> pure (ESeq cm e)) ) `composeL` promoteL)
            <+ missingChildL 1

  chooseL n = missingChildL n

---------------------------------------------------------------------------

instance Injection Cmd GenericExpr where
  inject = GCmd

  retract (GCmd c) = Just c
  retract _        = Nothing

instance Term Cmd where
  type Generic Cmd = GenericExpr

instance WalkerR Context Maybe Cmd where
  allR r =  seqT (extractR r) (extractR r) Seq
         <+ assignT (extractR r) Assign

  anyR r =  seqT' (attemptR $ extractR r) (attemptR $ extractR r) (attemptAny2 Seq)
         <+ assignT (extractR r) Assign
         <+ fail "anyR failed"

instance Monoid b => WalkerT Context Maybe Cmd b where
  crushT t =  seqT (extractT t) (extractT t) mappend
           <+ assignT (extractT t) (\ _ -> id)

instance WalkerL Context Maybe Cmd where
  chooseL 0 =  (seqT exposeT idR ( \ ccm1 cm2 -> (ccm1, \ cm1 -> pure (Seq cm1 cm2)) ) `composeL` promoteL)
            <+ (assignT exposeT ( \ v ce -> (ce, \ e -> pure (Assign v e)) ) `composeL` promoteL)

  chooseL 1 =  (seqT idR exposeT ( \ cm1 ccm2 -> (second inject ccm2, retractWithA (Seq cm1)) ) )
            <+ missingChildL 1

  chooseL n = missingChildL n

    -- n = lens $ \ c cm -> case cm of
    --                              Assign v e  ->  case n of
    --                                                0 -> pure ((c,GExpr e), retractWithA (Assign v))
    --                                                _ -> empty
    --                              Seq cm1 cm2 ->  case n of
    --                                                0 -> pure ((c,GCmd cm1), retractWithA (flip Seq cm2))
    --                                                1 -> pure ((updateContext cm1 c, GCmd cm2), retractWithA (Seq cm1))
    --                                                _ -> empty

-- chooseL0of1 :: (Translate c m a0 (c,b) -> ((c,b) -> ((c,b) , b -> m r))) -> (b -> r) -> Lens c m a r -- (Generic r)
-- chooseL0of1 comb f = comb exposeT (\ cb -> (cb, \ b -> pure (f b)) ) -- `composeL` promoteL

---------------------------------------------------------------------------

seqT' :: TranslateE Cmd a1 -> TranslateE Cmd a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Cmd b
seqT' t1 t2 f = translate $ \ c cm -> case cm of
                                       Seq cm1 cm2 -> f (apply t1 c cm1) (apply t2 (updateContext cm1 c) cm2)
                                       _           -> fail "not a Seq"

seqT :: TranslateE Cmd a1 -> TranslateE Cmd a2 -> (a1 -> a2 -> b) -> TranslateE Cmd b
seqT t1 t2 f = seqT' t1 t2 (liftA2 f)

assignT :: TranslateE Expr a -> (Name -> a -> b) -> TranslateE Cmd b
assignT t f = translate $ \ c cm -> case cm of
                                      Assign n e -> f n <$> apply t c e
                                      _          -> fail "not an Assign"

varT :: (Name -> b) -> TranslateE Expr b
varT f = liftMT $ \ e -> case e of
                           Var v -> pure (f v)
                           _     -> fail "not a Var"

litT :: (Int -> b) -> TranslateE Expr b
litT f = liftMT $ \ e -> case e of
                           Lit v -> pure (f v)
                           _     -> fail "not a Lit"

addT' :: TranslateE Expr a1 -> TranslateE Expr a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Expr b
addT' t1 t2 f = translate $ \ c e -> case e of
                                       Add e1 e2 -> f (apply t1 c e1) (apply t2 c e2)
                                       _         -> fail "not an Add"

addT :: TranslateE Expr a1 -> TranslateE Expr a2 -> (a1 -> a2 -> b) -> TranslateE Expr b
addT t1 t2 f = addT' t1 t2 (liftA2 f)

eseqT' :: TranslateE Cmd a1 -> TranslateE Expr a2 -> (Maybe a1 -> Maybe a2 -> Maybe b) -> TranslateE Expr b
eseqT' t1 t2 f = translate $ \ c e -> case e of
                                        ESeq cm e1 -> f (apply t1 c cm) (apply t2 (updateContext cm c) e1)
                                        _          -> fail "not an ESeq"

eseqT :: TranslateE Cmd a1 -> TranslateE Expr a2 -> (a1 -> a2 -> b) -> TranslateE Expr b
eseqT t1 t2 f = eseqT' t1 t2 (liftA2 f)

---------------------------------------------------------------------------
