module ExpExamples where

import Language.KURE

import Exp
import ExpKure

import Data.Monoid
import Control.Applicative
import Data.Maybe
import Data.List
-- import Debug.Trace

------------------------------------------------------------------------

-- exp1 :: Exp
-- exp1 = Var "x"

-- exp2 :: Exp
-- exp2 = Var "y"

-- exp3 :: Exp
-- exp3 = Lam "x" exp1

-- exp4 :: Exp
-- exp4 = Lam "x" exp2

-- exp5 :: Exp
-- exp5 = App exp1 exp2

-- exp6 :: Exp
-- exp6 = App exp3 exp4

-- exp7 :: Exp
-- exp7 = App exp4 exp6

-- exp8 :: Exp
-- exp8 = Lam "z" (Var "z")

-- exp9 :: Exp
-- exp9 = Lam "x" exp3

-- exp10 :: Exp
-- exp10 = Lam "x" exp4

-- exp11 :: Exp
-- exp11 = Lam "x" exp5

------------------------------------------------------------------------

-- expTest :: IO ()
-- expTest = do
-- 	let es1 = [exp1,exp2,exp3,exp4,exp5,exp6,exp7,exp8,exp9,exp10,exp11]
-- 	print "all expressions"
-- 	mapM_ print es1

-- 	let frees = map freeVars es1
-- 	mapM print (zip es1 frees)

--         sequence_ [ print (e,applyExp (substExp v ed) e)  | v <- ["x","y","z"], ed <- es1, e <- es1 ]

--         mapM (print . applyExp (tryR betaRedR)) es1

--         let fn1 = extractR (alltdR (repeatR betaRedR))
--         mapM (print . applyExp fn1) es1

-- 	let fn2 = liftT ( \ (Var x) -> Var ('!':x))
-- 	print (applyExp fn2 (Var "abc"))

-- 	let fn3 = liftT ( \ (Var x) -> Var ('!':x)) <+ pure (Var "X")
-- 	print (applyExp fn3 (Var "abc"))

------------------------------------------------------------------------

freeVarsT :: TranslateExp [Name]
freeVarsT = fmap nub $ crushbuT $ mtryT $ do (c, Var v) <- exposeT
                                             return (if v `elem` c then [] else [v])

freeVars :: Exp -> [Name]
freeVars = fromJust . applyExp freeVarsT

-- Only works for lambdas, fails for all others
alphaLam :: [Name] -> RewriteExp
alphaLam frees = do Lam n e <- idR
                    n' <- constMT $ freshName $ frees ++ n : freeVars e
                    lamT (substExp n $ Var n') (\ _ -> Lam n')

--                                                            e' <- apply (substExp n (Var n')) (n:n':c) e

substExp :: Name -> Exp -> RewriteExp
substExp v s = rules_var <+ rules_lam <+ rule_app
 where
        -- From Lambda Calc Textbook, the 6 rules.
        rules_var = varT $ \ n -> if v == n
                                   then s                                       -- Rule 1
                                   else Var n                                   -- Rule 2

        rules_lam = do Lam n e <- idR
                       whenT (n == v) idR                                       -- Rule 3
                        <+ whenT (v `notElem` freeVars e) idR                   -- Rule 4a
                        <+ whenT (n `notElem` freeVars s) (allR (substExp v s)) -- Rule 4b
                        <+ (alphaLam (freeVars s) >-> rules_lam)                -- Rule 5

        rule_app = do App _ _ <- idR
                      allR (substExp v s)                                       -- Rule 6

------------------------------------------------------------------------

beta_reduce :: RewriteExp
beta_reduce = rewrite $ \ c e -> case e of
                                App (Lam v e1) e2 -> apply (substExp v e2) (v:c) e1
                                _                 -> fail "Cannot beta-reduce, not applying a lambda."

eta_expand :: Name -> RewriteExp
eta_expand nm = rewrite $ \ c f -> do v <- freshName c
                                      return $ Lam v (App f (Var v))

eta_reduce :: RewriteExp
eta_reduce = do Lam v1 (App f (Var v2)) <- idR
                guardT (v1 == v2) $ "Cannot eta-reduce, " ++ v1 ++ " /= " ++ v2
                return f

------------------------------------------------------------------------

-- debugR :: String -> RewriteExp
-- debugR msg = liftT $ \ e -> trace (msg ++ " : " ++ show e) e

------------------------------------------------------------------------

vX :: Exp
vX = Var "x"

vY :: Exp
vY = Var "y"

vZ :: Exp
vZ = Var "z"

------------------------------------------------------------------------

-- exp3 :: Exp
-- exp3 = Lam "x" exp1

-- exp4 :: Exp
-- exp4 = Lam "x" exp2

-- exp5 :: Exp
-- exp5 = App exp1 exp2

-- exp6 :: Exp
-- exp6 = App exp3 exp4

-- exp7 :: Exp
-- exp7 = App exp4 exp6

-- exp8 :: Exp
-- exp8 = Lam "z" (Var "z")

-- exp9 :: Exp
-- exp9 = Lam "x" exp3

-- exp10 :: Exp
-- exp10 = Lam "x" exp4

-- exp11 :: Exp
-- exp11 = Lam "x" exp5

------------------------------------------------------------------------
