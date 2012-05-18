module Exp where

import Control.Applicative
import Control.Monad

-------------------------------------------------------------------------------

import Language.KURE.Utilities (result,second)

type Name = String

data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name
           deriving Eq

instance Show Exp where
  show (Var v)   = v
  show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Lam n x) = "(\\" ++ n ++ ". " ++ show x ++ ")"

-------------------------------------------------------------------------------

type Context = [Name] -- bound variable names

newtype ExpM a = ExpM {expM :: Int -> (Int, Either String a)}

runExpM :: ExpM a -> Either String a
runExpM m = snd (expM m 0)

instance Functor ExpM where
  fmap f (ExpM m) = ExpM ((result.second.fmap) f m)

instance Monad ExpM where
  return a = ExpM (\n -> (n,Right a))
  (ExpM f) >>= gg = ExpM $ \ n -> case f n of
                                    (n', Left msg) -> (n', Left msg)
                                    (n', Right a)  -> expM (gg a) n'
  fail msg = ExpM (\ n -> (n, Left msg))

instance MonadPlus ExpM where
  mzero = fail ""
  (ExpM f) `mplus` (ExpM g) = ExpM $ \ n -> case f n of
                                              (n', Left msg) -> g n'
                                              (n', Right a)  -> (n', Right a)

instance Applicative ExpM where
  pure  = return
  (<*>) = ap

instance Alternative ExpM where
  empty = mzero
  (<|>) = mplus

-------------------------------------------------------------------------------

suggestName :: ExpM Name
suggestName = ExpM (\n -> ((n+1), Right (show n)))

freshName :: Context -> ExpM Name
freshName c = do n <- suggestName
                 if n `elem` c
                  then freshName c
                  else return n

-------------------------------------------------------------------------------
