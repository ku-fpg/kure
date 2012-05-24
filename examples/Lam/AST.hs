module Lam.AST where

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

newtype LamM a = LamM {expM :: Int -> (Int, Either String a)}

runLamM :: LamM a -> Either String a
runLamM m = snd (expM m 0)

instance Functor LamM where
  fmap f (LamM m) = LamM ((result.second.fmap) f m)

instance Monad LamM where
  return a = LamM (\n -> (n,Right a))
  (LamM f) >>= gg = LamM $ \ n -> case f n of
                                    (n', Left msg) -> (n', Left msg)
                                    (n', Right a)  -> expM (gg a) n'
  fail msg = LamM (\ n -> (n, Left msg))

instance MonadPlus LamM where
  mzero = fail ""
  (LamM f) `mplus` (LamM g) = LamM $ \ n -> case f n of
                                              (n', Left _)  -> g n'
                                              (n', Right a) -> (n', Right a)

instance Applicative LamM where
  pure  = return
  (<*>) = ap

instance Alternative LamM where
  empty = mzero
  (<|>) = mplus

-------------------------------------------------------------------------------

suggestName :: LamM Name
suggestName = LamM (\n -> ((n+1), Right (show n)))

freshName :: Context -> LamM Name
freshName c = do n <- suggestName
                 if n `elem` c
                  then freshName c
                  else return n

-------------------------------------------------------------------------------
