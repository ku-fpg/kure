{-# LANGUAGE MultiParamTypeClasses #-}

module Expr.Context where

import Data.Monoid (mempty)

import Language.KURE

import Expr.AST

---------------------------------------------------------------------------

data Context = Context (AbsolutePath Int) [(Name,Expr)] -- A list of bindings.
                                                        -- We assume no shadowing in the language.

instance ExtendPath Context Int where
-- (@@) :: Context -> Int -> Context
   (Context p defs) @@ n = Context (p @@ n) defs

instance ReadPath Context Int where
-- absPath :: Context -> AbsolutePath
   absPath (Context p _) = p

class AddDef c where
  addDef :: Name -> Expr -> c -> c

updateContextCmd :: AddDef c => Cmd -> c -> c
updateContextCmd (Seq c1 c2)  = updateContextCmd c2 . updateContextCmd c1
updateContextCmd (Assign v e) = (addDef v e)

instance AddDef (SnocPath crumb) where
-- addDef :: Name -> Expr -> SnocPath crumb -> SnocPath crumb
   addDef _ _ = id

instance AddDef Context where
-- addDef :: Name -> Expr -> Context -> Context
   addDef v e (Context p defs) = Context p ((v,e):defs)

initialContext :: Context
initialContext = Context mempty []

lookupDef :: Monad m => Name -> Context -> m Expr
lookupDef v (Context _ defs) = maybe (fail $ v ++ " not found in context") return (lookup v defs)

---------------------------------------------------------------------------