{-# LANGUAGE InstanceSigs, MultiParamTypeClasses #-}
module Lam.Context where

import Data.Monoid (mempty)

import Language.KURE

import Lam.AST (Name)

-------------------------------------------------------------------------------

data Crumb = Lam_Body
           | Lam_Name
           | App_Fun
           | App_Arg
             deriving (Eq, Show)

-- The context
data LamC = LamC (AbsolutePath Crumb) [Name] -- bound variable names

class AddBoundVar c where
  addBoundVar :: Name -> c -> c

instance AddBoundVar LamC where
   addBoundVar :: Name -> LamC -> LamC
   addBoundVar v (LamC p vs) = LamC p (v:vs)

instance ExtendPath LamC Crumb where
   (@@) :: LamC -> Crumb -> LamC
   (LamC p vs) @@ cr = LamC (p @@ cr) vs

instance ReadPath LamC Crumb where
   absPath :: LamC -> AbsolutePath Crumb
   absPath (LamC p _) = p

initialLamC :: LamC
initialLamC = LamC mempty []

bindings :: LamC -> [Name]
bindings (LamC _ vs) = vs

boundIn :: Name -> LamC -> Bool
boundIn v c = v `elem` bindings c

freeIn :: Name -> LamC -> Bool
freeIn v c = not (v `boundIn` c)

-------------------------------------------------------------------------------
