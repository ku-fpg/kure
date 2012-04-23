module ExpTest where

-- WARNING: The logic of this module may have been damaged when updating it to the latest version of KURE
-- shallowAlpha and substExp need checking

import Language.KURE

import Exp
import ExpInstances()

import Data.Monoid
import Control.Applicative
import Data.Maybe
import Data.List
import Debug.Trace

type R e   = Rewrite   Context ExpM e
type T e b = Translate Context ExpM e b

-- examples

exp1 :: Exp
exp1 = Var "x"

exp2 :: Exp
exp2 = Var "y"

exp3 :: Exp
exp3 = Lam "x" exp1

exp4 :: Exp
exp4 = Lam "x" exp2

exp5 :: Exp
exp5 = App exp1 exp2

exp6 :: Exp
exp6 = App exp3 exp4

exp7 :: Exp
exp7 = App exp4 exp6

exp8 :: Exp
exp8 = Lam "z" (Var "z")

exp9 :: Exp
exp9 = Lam "x" exp3

exp10 :: Exp
exp10 = Lam "x" exp4

exp11 :: Exp
exp11 = Lam "x" exp5


expTest :: IO ()
expTest = do
	let es1 = [exp1,exp2,exp3,exp4,exp5,exp6,exp7,exp8,exp9,exp10,exp11]
	print "all expressions"
	mapM_ print es1
        
	let frees = map freeVars es1
	mapM_ print (zip es1 frees)

        sequence_ [ print (e,function (substExp v ed) e)  | v <- ["x","y","z"], ed <- es1, e <- es1 ]

        sequence_ [ print (function (tryR betaRedR) e) | e <- es1 ]
        
        let fn1 = extractR (alltdR (repeatR betaRedR))
        sequence_ [ print (function fn1 e) | e <- es1 ]

	let fn2 = rewrite (\ _ (Var x) -> return (Var ('!':x)))
	print (function fn2 (Var "abc"))
        
	let fn3 = rewrite (\ _ (Var x) -> return (Var ('!':x))) <+ constT (Var "X")
	print (function fn3 (Var "abc"))


function :: T a b -> a -> Maybe b
function f = runExpM . apply f []

------------------------------------------------------------------------

freeVarsT :: T Exp [Name]
freeVarsT = liftA nub (lambda <+ var <+ crushT freeVarsT)
  where
          var    = varG >-> translate (\ c (Var n) -> pure (if n `elem` c then [] else [n]))
          lambda = lamG >-> translate (\ c (Lam n e) -> apply freeVarsT (n:c) e)
                                                        
freeVars :: Exp -> [Name]
freeVars = fromJust . function freeVarsT

-- Only works for lambdas, fails for all others
shallowAlpha :: [Name] -> R Exp
shallowAlpha frees = lamG >-> rewrite (\ c (Lam n e) -> do n' <- newName (freeVars e ++ frees)
                                                           e' <- apply (substExp n (Var n')) (n:n':c) e
                                                           return (Lam n' e'))
                     
substExp :: Name -> Exp -> R Exp
substExp v s = rule1 <+ rule2 <+ rule3 <+ rule4 <+ rule5 <+ rule6
 where
        -- From Lambda Calc Textbook, the 6 rules.
        rule1 = varP $ \ n -> whenT (n == v) (constT s)
        rule2 = varP $ \ n -> guardR (n /= v)
        rule3 = lamP $ \ n _ -> guardR (n == v)
        rule4 = lamP $ \ n e -> whenT (n `notElem` freeVars s || v `notElem` freeVars e) 
                                      (allR (substExp v s))
        rule5 = lamP $ \ n e -> whenT (n `elem` freeVars s && v `elem` freeVars e)
                                      (shallowAlpha (freeVars s) >-> substExp v s)
        rule6 = appG >-> allR (substExp v s)

-------------

betaRedR :: R Exp
betaRedR = rewrite $ \ c e -> case e of
                                App (Lam v e1) e2 -> apply (substExp v e2) (v:c) e1
                                _                 -> empty

debugR :: (Show e) => String -> R e      
debugR msg = translate $ \ _ e -> trace (msg ++ " : " ++ show e) (pure e)


------------------------------------------------------------------------
--
-- First the guards
--

appG :: R Exp
appG = acceptR $ \ e -> case e of { App {} -> True ; _ -> False }

lamG :: R Exp
lamG = acceptR $ \ e -> case e of { Lam {} -> True ; _ -> False }

varG :: R Exp
varG = acceptR $ \ e -> case e of { Var {} -> True; _ -> False }

------------------------------------------------------------------------
--
-- Then the rewrites and traversals
--

appR :: R Exp -> R Exp -> R Exp
appR r1 r2 = appG >-> rewrite (\ c (App e1 e2) -> App <$> apply r1 c e1 <*> apply r2 c e2) 

lamR :: R Exp -> R Exp
lamR r = lamG >-> rewrite (\ c (Lam n e) -> Lam n <$> apply r c e)
                                           
varR :: R Exp
varR = varG

appT :: (Monoid r) => T Exp r -> T Exp r -> T Exp r
appT r1 r2 = appG >-> translate (\ c (App e1 e2) -> mappend <$> apply r1 c e1 <*> apply r2 c e2) 

lamT :: (Monoid r) => T Exp r -> T Exp r
lamT t = lamG >-> translate (\ c (Lam _ e) -> apply t c e)
                                           
varT :: (Monoid r) => T Exp r
varT = varG >-> emptyT


------------------------------------------------------------------------
--
-- Finally, the pattern matches
--

appP ::(Exp -> Exp -> T Exp r) -> T Exp r
appP f = appG >-> readerT (\ (App e1 e2) -> f e1 e2) 

lamP ::  (Name -> Exp -> T Exp r) -> T Exp r
lamP f = lamG >-> readerT (\ (Lam n e) -> f n e)

varP :: (Name -> T Exp r) -> T Exp r
varP f = varG >-> readerT (\ (Var n) -> f n)

------------------------------------------------------------------------
