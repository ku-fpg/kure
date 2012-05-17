module Exp where

import Control.Applicative
import Control.Monad
import Control.Arrow

-------------------------------------------------------------------------------

import Language.KURE.Utilities (result)

type Name = String

data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name

instance Show Exp where
  show (Var v)   = v
  show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Lam n x) = "(\\" ++ n ++ ". " ++ show x ++ ")"

-------------------------------------------------------------------------------

type Context = [Name] -- bound variable names

newtype ExpM a = ExpM {expM :: Int -> (Int, Maybe a)}

runExpM :: ExpM a -> Maybe a
runExpM m = snd (expM m 0)

instance Functor ExpM where
  fmap f (ExpM m) = ExpM ((result.second.fmap) f m)

instance Monad ExpM where
  return a = ExpM (\n -> (n,Just a))
  (ExpM f) >>= gg = ExpM $ \ n -> case f n of
                                    (n', Nothing) -> (n', Nothing)
                                    (n', Just a)  -> case gg a of
                                                       ExpM g -> g n'
  fail _ = empty

instance Applicative ExpM where
  pure  = return
  (<*>) = ap

instance Alternative ExpM where
  empty = ExpM (\ n -> (n,Nothing))
  (ExpM f) <|> (ExpM g) = ExpM $ \ n -> case f n of
                                          (n', Nothing) -> g n'
                                          (n', Just a)  -> (n', Just a)

instance MonadPlus ExpM where
  mzero = empty
  mplus = (<|>)

-------------------------------------------------------------------------------

suggestName :: ExpM Name
suggestName = ExpM (\n -> ((n+1), Just (show n)))

freshName :: Context -> ExpM Name
freshName c = do n <- suggestName
                 if n `elem` c
                  then freshName c
                  else return n

-------------------------------------------------------------------------------
