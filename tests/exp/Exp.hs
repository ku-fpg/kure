module Exp where

import Control.Applicative
import Control.Monad
import Control.Arrow

type Name = String

data Exp = Lam Name Exp
         | App Exp Exp
         | Var Name
           deriving (Show,Eq)

-- examples
e1 = Var "x"
e2 = Var "y"
e3 = Lam "x" e1
e4 = Lam "x" e2
e5 = App e1 e2
e6 = App e3 e4
e7 = App e4 e6
e8 = Lam "z" (Var "z")
e9 = Lam "x" e3
e10 = Lam "x" e4
e11 = Lam "x" e5


type Context = [Name] -- bound variable names

newtype ExpM a = ExpM (Int -> (Int, Maybe a)) 

instance Functor ExpM where
  fmap f (ExpM m) = ExpM (second (fmap f) . m)

instance Monad ExpM where  
  return a = ExpM (\n -> (n,Just a))
  (ExpM f) >>= gg = ExpM $ \ n -> case f n of
                                    (n',Nothing) -> (n', Nothing)
                                    (n',Just a)  -> case gg a of  
                                                      ExpM g -> g n'
  
instance Applicative ExpM where
  pure  = return  
  (<*>) = ap
  
instance Alternative ExpM where
  empty = ExpM (\n -> (n,Nothing))
  (ExpM f) <|> (ExpM g) = ExpM $ \ n -> case f n of
                                          (n',Nothing) -> g n'
                                          (n',Just a)  -> (n', Just a)
    
suggestName :: ExpM Name
suggestName = ExpM (\n -> ((n+1), Just (show n)))

newName :: Context -> ExpM Name
newName c = do n <- suggestName
               if n `elem` c
                then newName c  
                else return n
