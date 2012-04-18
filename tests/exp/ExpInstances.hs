{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module ExpInstances where

import Language.KURE
import Exp

import Data.Monoid
import Control.Applicative

instance Term Exp where
   type Generic Exp = Exp  -- Exp is its own Generic root.
  
instance WalkerR Context ExpM Exp where
  
   allR r = rewrite $ \ c e -> case e of 
                                 Var v     -> pure (Var v)
                                 App e1 e2 -> App <$> apply r c e1 <*> apply r c e2
                                 Lam v e   -> Lam v <$> apply r (v:c) e

instance Monoid b => WalkerT Context ExpM Exp b where
  
   crushT t = translate $ \ c e -> case e of
                                     Var v     -> pure mempty
                                     App e1 e2 -> mappend <$> apply t c e1 <*> apply t c e2
                                     Lam v e   -> apply t (v:c) e

instance WalkerL Context ExpM Exp where

   chooseL n = lens $ \ c e -> case e of
                                Var v      ->  empty
                                App e1 e2  ->  case n of
                                                 0 -> pure ((c,e1), \ e1' -> pure (App e1' e2))
                                                 1 -> pure ((c,e2), \ e2' -> pure (App e1 e2'))
                                                 _ -> empty
                                Lam v e    ->  case n of
                                                 0 -> pure ((v:c,e), \ e' -> pure (Lam v e'))
                                                 _ -> empty