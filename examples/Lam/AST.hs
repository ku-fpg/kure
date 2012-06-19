module Lam.AST where

import Control.Applicative
import Control.Monad

import Language.KURE

-------------------------------------------------------------------------------

import Control.Arrow (second)

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

data Context = Context AbsolutePath [Name] -- bound variable names

instance PathContext Context where
  contextPath (Context p _) = p

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

suggestName :: LamM Name
suggestName = LamM (\n -> ((n+1), Right (show n)))

freshName :: [Name] -> LamM Name
freshName vs = do v <- suggestName
                  if v `elem` vs
                    then freshName vs
                    else return v

-------------------------------------------------------------------------------
