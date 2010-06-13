{-# LANGUAGE TypeFamilies #-}

module ExpTest where

import Language.KURE

import Exp
import ExpInstances

import Data.Monoid
import Control.Monad
import Data.List
import Debug.Trace

expTest = do
	let es1 = [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11]
	print "all expressions"
	sequence_ [ print e | e <- es1]

	let frees :: Exp -> IO [Name]
	    frees exp = do fs <- apply freeExpT exp
			   return $ nub fs
	e_frees <- mapM frees es1
	sequence_ [ print (e,f) | (e,f) <- zip es1 e_frees ]

        sequence_ [ print (e,function (substExp v ed) e)  | v <- ["x","y","z"], ed <- es1, e <- es1 ]

        sequence_ [ print (function (tryR betaRedR) e) | e <- es1 ]
        let fn = extractR (topdownR (repeatR betaRedR))
        sequence_ [ print (function fn e) | e <- es1 ]

	let fn = idR .+ (rewrite $ (\ (Var x) -> return $ Var ('!':x)))
	sequence_ [ print (function fn (Var "abc")) ]
	let fn = (constT (Var "T") .+ (rewrite $ (\ (Var x) -> return $ Var ('!':x))))
		<+ constT (Var "X")
	sequence_ [ print (function fn (Var "abc")) ]


function :: Translate a b -> a -> Maybe b
function f a = apply f a

------------------------------------------------------------------------

freeExpT :: T Exp [Name]
freeExpT = lambda <+ var <+ crushU freeExpT
  where
          var    = varG >-> translate (\ (Var v) -> return [v])
          lambda = lamG >-> translate (\ (Lam n e) -> do
                frees <- apply freeExpT e
                return (nub frees \\ [n]))
                
freeExp :: Exp -> [Name]
freeExp e = case function freeExpT e of
	     Nothing -> error "bad freeExp"
	     Just v -> v

newName :: Name -> [Name] -> Name
newName suggest frees = 
        head [ nm | nm <- suggest : suggests
             , nm `notElem` frees
             ]
   where suggests = [ suggest ++ "_" ++ show n | n <- [1..]]

-- Only works for lambdas, fails for all others
shallowAlpha :: [Name] -> R Exp
shallowAlpha frees' = lamG >-> 
                        rewrite (\ (Lam n e) -> do
                frees <- apply freeExpT e
                let n' = newName n (frees ++ frees')
                e' <- apply (substExp n (Var n')) e
                return $ Lam n' e') 

substExp :: Name -> Exp -> R Exp
substExp v s = rule1 <+ rule2 <+ rule3 <+ rule4 <+ rule5 <+ rule6
 where
        -- From Lambda Calc Textbook, the 6 rules.
        rule1 = varP $ \ n -> n == v ? constT s
        rule2 = varP $ \ n -> n /= v ? idR
        rule3 = lamP $ \ n e -> n == v ? idR
        rule4 = lamP $ \ n e -> (n `notElem` freeExp s || v `notElem` freeExp e) 
                                ? allR (substExp v s)
        rule5 = lamP $ \ n e -> (n `elem` freeExp s && v `elem` freeExp e)
                                ? (shallowAlpha (freeExp s) >-> substExp v s)
        rule6 = appG >-> allR (substExp v s)

-------------

betaRedR :: R Exp
betaRedR = rewrite $ \ e ->
   case e of
     (App (Lam v e1) e2) -> apply (substExp v e2) e1
     _ -> fail "betaRed"

debugR :: (Show e) => String -> R e      
debugR msg = translate $ \ e -> trace (msg ++ " : " ++ show e) (return e)

