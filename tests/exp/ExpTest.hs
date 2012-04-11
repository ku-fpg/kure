module ExpTest where

-- WARNING: The logic of this module may have been damaged when updating it to the latest version of KURE
-- shallowAlpha and substExp need checking

import Language.KURE

import Exp
import ExpInstances

import Data.Monoid
import Control.Applicative
import Data.Maybe
import Data.List
import Debug.Trace

type R e   = Rewrite   Context ExpM e
type T e b = Translate Context ExpM e b

expTest :: IO ()
expTest = do
	let es1 = [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11]
	print "all expressions"
	mapM_ print es1
        
	let frees = map freeVars es1
	mapM_ print (zip es1 frees)

        sequence_ [ print (e,function (substExp v ed) e)  | v <- ["x","y","z"], ed <- es1, e <- es1 ]

        sequence_ [ print (function (tryR betaRedR) e) | e <- es1 ]
        
        let fn = extractR (topdownR (repeatR betaRedR))
        sequence_ [ print (function fn e) | e <- es1 ]

	let fn = rewrite (\ _ (Var x) -> return (Var ('!':x)))
	print (function fn (Var "abc"))
        
	let fn = rewrite (\ _ (Var x) -> return (Var ('!':x))) <+ constT (Var "X")
	print (function fn (Var "abc"))


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
        rule1 = varP $ \ n -> n == v ? constT s
        rule2 = varP $ \ n -> n /= v ? idR
        rule3 = lamP $ \ n e -> n == v ? idR
        rule4 = lamP $ \ n e -> (n `notElem` freeVars s || v `notElem` freeVars e) 
                                ? allR (substExp v s)
        rule5 = lamP $ \ n e -> (n `elem` freeVars s && v `elem` freeVars e)
                                ? (shallowAlpha (freeVars s) >-> substExp v s)
        rule6 = appG >-> allR (substExp v s)

-------------

betaRedR :: R Exp
betaRedR = rewrite $ \ c e -> case e of
                                App (Lam v e1) e2 -> apply (substExp v e2) (v:c) e1
                                _                 -> empty

debugR :: (Show e) => String -> R e      
debugR msg = translate $ \ c e -> trace (msg ++ " : " ++ show e) (pure e)


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
appR r1 r2 = appG >-> rewrite (\ c (App e1 e2) -> liftA2 App (apply r1 c e1) 
                                                             (apply r2 c e2)) 

lamR :: R Exp -> R Exp
lamR r = lamG >-> rewrite (\ c (Lam n e) -> liftA (Lam n) (apply r c e))
                                           
varR :: R Exp
varR = varG

appT :: (Monoid r) => T Exp r -> T Exp r -> T Exp r
appT r1 r2 = appG >-> translate (\ c (App e1 e2) -> liftA2 mappend (apply r1 c e1) 
                                                                   (apply r2 c e2)) 

lamT :: (Monoid r) => T Exp r -> T Exp r
lamT t = lamG >-> translate (\ c (Lam n e) -> apply t c e)
                                           
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
