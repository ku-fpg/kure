module Exp where


-- Using de Bruijn Indices
type Index = Int

data Exp = Lam Exp
         | App Exp Exp
         | Var Index
         deriving (Show,Eq)

-- examples
e1 = Var 0
e2 = Var 1
e3 = Lam e1
e4 = Lam e2
e5 = App e1 e2
e6 = App e3 e4
e7 = App e4 e6
e8 = Lam (Var 3)
e9 = Lam e3
e10 = Lam e4
e11 = Lam e5


type Context = [Exp]

lookupVar :: Index -> Context -> Maybe Exp
lookupVar _ []     = Nothing
lookupVar 0 (e:_)  = Just e
lookupVar n (_:es) = lookupVar n es

underLam :: Context -> Context
underLam c = map (incrVarFrom 0) c

incrVarFrom :: Index -> Exp -> Exp
incrVarFrom i (Lam e)     = Lam (incrVarFrom (succ i) e)
incrVarFrom i (App e1 e2) = App (incrVarFrom i e1) (incrVarFrom i e2) 
incrVarFrom i (Var v)     = Var (if v >= i then succ v else v)
