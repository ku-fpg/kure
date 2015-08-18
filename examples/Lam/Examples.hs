module Lam.Examples where

import Language.KURE

import Lam.AST
import Lam.Kure
import Lam.Context
import Lam.Monad

import Data.List (nub)

import Control.Category ((>>>))

-------------------------------------------------------------------------------

suggestName :: LamM Name
suggestName = LamM (\n -> ((n+1), Right (show n)))

freshName :: [Name] -> LamM Name
freshName vs = do v <- suggestName
                  if v `elem` vs
                    then freshName vs
                    else return v

-------------------------------------------------------------------------------

type TransformE b = Transform LamC LamM Exp b
type RewriteE     = TransformE Exp

-------------------------------------------------------------------------------

applyExp :: TransformE b -> Exp -> Either String b
applyExp t = either (Left . show) Right . runLamM . applyT t initialLamC

------------------------------------------------------------------------

freeVarsT :: TransformE [Name]
freeVarsT = fmap nub $ crushbuT $ do (c, Var v) <- exposeT
                                     guardM (v `freeIn` c)
                                     return [v]

freeVars :: Exp -> [Name]
freeVars = either error id . applyExp freeVarsT

-- Only works for lambdas, fails for all others
alphaLam :: [Name] -> RewriteE
alphaLam frees = do Lam v e <- idR
                    v' <- constT $ freshName $ frees ++ v : freeVars e
                    lamT (tryR $ substExp v (Var v')) (\ _ -> Lam v')

substExp :: Name -> Exp -> RewriteE
substExp v s = rules_var <+ rules_lam <+ rule_app
 where
        -- From Lambda Calc Textbook, the 6 rules.
        rules_var = whenM (varT (==v)) (return s)                   -- Rule 1

        rules_lam = do Lam n e <- idR
                       guardM (n /= v)                              -- Rule 3
                       guardM (v `elem` freeVars e)                 -- Rule 4a
                       if n `elem` freeVars s
                        then alphaLam (freeVars s) >>> rules_lam    -- Rule 5
                        else lamR (substExp v s)                    -- Rule 4b

        rule_app = do App _ _ <- idR
                      anyR (substExp v s)                           -- Rule 6

------------------------------------------------------------------------

beta_reduce :: RewriteE
beta_reduce = withPatFailExc (nodeMismatch "cannot beta-reduce, not app-lambda.") $
                do App (Lam v _) e2 <- idR
                   pathT [App_Fun,Lam_Body] (tryR $ substExp v e2)

eta_expand :: RewriteE
eta_expand = rewrite $ \ c f -> do v <- freshName (bindings c)
                                   return $ Lam v (App f (Var v))

eta_reduce :: RewriteE
eta_reduce = withPatFailExc (nodeMismatch "cannot eta-reduce, not lambda-app-var.") $
               do Lam v1 (App f (Var v2)) <- idR
                  guardExc (v1 == v2) . conditionalFailure $
                    "Cannot eta-reduce, " ++ v1 ++ " /= " ++ v2
                  return f

-- This might not actually be normal order evaluation
-- Contact the  KURE maintainer if you can correct this definition.
normal_order_eval :: RewriteE
normal_order_eval = anytdR (repeatR beta_reduce)

-- This might not actually be applicative order evaluation
-- Contact the KURE maintainer if you can correct this definition.
applicative_order_eval :: RewriteE
applicative_order_eval = innermostR beta_reduce

------------------------------------------------------------------------

type LamTest = (RewriteE, String, Exp, Maybe Exp)

runLamTest :: LamTest -> (Bool, String)
runLamTest (r,_,e,me) = case (applyExp r e , me) of
                        (Right r1 , Just r2) | r1 == r2 -> (True, show r1)
                        (Left msg , Nothing)            -> (True, msg)
                        (Left msg , Just _)             -> (False, msg)
                        (Right r1 , _     )             -> (False, show r1)

ppLamTest :: LamTest -> IO ()
ppLamTest t@(_,n,e,me) = do putStrLn $ "Rewrite: " ++ n
                            putStrLn $ "Initial expression: " ++ show e
                            putStrLn $ "Expected outcome: " ++ maybe "failure" show me
                            let (b,msg) = runLamTest t
                            putStrLn $ "Actual outcome: " ++ msg
                            putStrLn (if b then "TEST PASSED" else "TEST FAILED")
                            putStrLn ""

------------------------------------------------------------------------

x :: Exp
x = Var "x"

y :: Exp
y = Var "y"

z :: Exp
z = Var "z"

g :: Exp
g = Var "g"

h :: Exp
h = Var "h"

gx :: Exp
gx = App g x

gy :: Exp
gy = App g y

gz :: Exp
gz = App g z

hz :: Exp
hz = App h z

g0 :: Exp
g0 = App g (Var "0")

xx :: Exp
xx = App x x

yy :: Exp
yy = App y y

xz :: Exp
xz = App x z

fix :: Exp
fix = Lam "g" (App body body)
  where
    body = Lam "x" (App g xx)

------------------------------------------------------------------------

test_eta_exp1 :: LamTest
test_eta_exp1 = (eta_expand, "eta-expand", g, Just (Lam "0" g0))

test_eta_exp2 :: LamTest
test_eta_exp2 = (eta_expand, "eta-expand", App (Lam "g" gx) (Lam "y" yy), Just (Lam "0" (App (App (Lam "g" gx) (Lam "y" yy)) (Var "0"))))

test_eta_red1 :: LamTest
test_eta_red1 = (eta_reduce, "eta-reduce", Lam "x" gx , Just g)

test_eta_red2 :: LamTest
test_eta_red2 = (eta_reduce, "eta-reduce", Lam "x" gy, Nothing)

test_eta_red3 :: LamTest
test_eta_red3 = (eta_reduce, "eta-reduce", g, Nothing)

test_beta_red1 :: LamTest
test_beta_red1 = (beta_reduce, "beta-reduce", App (Lam "x" gx) z, Just gz)

test_beta_red2 :: LamTest
test_beta_red2 = (beta_reduce, "beta-reduce", App (Lam "x" gy) z, Just gy)

test_beta_red3 :: LamTest
test_beta_red3 = (beta_reduce, "beta-reduce", App x (Lam "y" gy), Nothing)

test_beta_reds1 :: LamTest
test_beta_reds1 = (anybuR beta_reduce, "any bottom-up beta-reduce", gx, Nothing)

test_beta_reds2 :: LamTest
test_beta_reds2 = (anybuR beta_reduce, "any bottom-up beta-reduce", App (Lam "g" gx) (Lam "h" (App h (App (Lam "y" y) z)))
                                                                  , Just (App (Lam "h" hz) x))

test_beta_reds3a :: LamTest
test_beta_reds3a = (beta_reduce, "beta-reduce", App (Lam "g" gx) (Lam "h" (App h (App (Lam "y" y) z)))
                                              , Just (App (Lam "h" (App h (App (Lam "y" y) z))) x))

test_beta_reds3 :: LamTest
test_beta_reds3 = (normal_order_eval, "normal order evaluation", App (Lam "g" gx) (Lam "h" (App h (App (Lam "y" y) z)))
                                                               , Just xz)

test_beta_reds4 :: LamTest
test_beta_reds4 = (applicative_order_eval, "applicative order evaluation", App (Lam "g" gx) (Lam "h" (App h (App (Lam "y" y) z)))
                                                                         , Just xz)

test_fix1 :: LamTest
test_fix1 = (normal_order_eval, "normal order evaluation", App fix (Lam "_" x), Just x)

diverge :: Either String Exp
diverge = applyExp applicative_order_eval (App fix (Lam "_" x))

test_fix2 :: LamTest
test_fix2 = (anybuR (andR $ replicate 3 $ anybuR beta_reduce), "applicative order evaluation - 3 step cap", App fix (Lam "_" x)
                                                             , Just (App (Lam "g" (App g (App g (App g (App g (App g (App g (App (Lam "x" (App g xx)) (Lam "x" (App g xx))))))))))
                                                                    (Lam "_" x))
                                                             )

all_tests :: [LamTest]
all_tests =    [ test_eta_exp1
               , test_eta_exp2
               , test_eta_red1
               , test_eta_red2
               , test_eta_red3
               , test_beta_red1
               , test_beta_red2
               , test_beta_red3
               , test_beta_reds1
               , test_beta_reds2
               , test_beta_reds3
               , test_beta_reds4
               , test_fix1
               , test_fix2
               ]

checkTests :: Bool
checkTests = all (fst . runLamTest) all_tests

printTests :: IO ()
printTests = mapM_ ppLamTest all_tests

------------------------------------------------------------------------
