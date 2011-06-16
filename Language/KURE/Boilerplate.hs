{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module: Language.KURE.Boilerplate
-- Copyright: (c) 2010 The University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module contains a Template Haskell based generator for the many data-type specific
-- functions that KURE want users to write. KURE Your Boilerplate (KYB) attempts to make
-- writing these function easy. Eventually, there will be a small DSL for effects inside the
-- generated functions.
--
-- Unfortunately, you still need to write the 'Term' instance by hand, because of the use of
-- type families, a feature that post-dates Template Haskell. You also need to write
-- the single MyGeneric datatype, which is considered documentation of what you want
-- KYB to do.
--
--  'kureYourBoilerplate' generates a 'Walker' instance for every data-type mentioned in your Generic,
-- a 'Walker' instance for the Generic type itself,
-- and the following for every constructor in every data-structure that is mentioned in Generic.
-- For example if a constructor is called 'Foo', and has type @Foo :: A -> B -> C@, we generate
--
--  * @fooR :: (...) => R A -> R B -> R C --@ congruence over @Foo@.
--
--  * @fooU :: (...,Monoid r) => T A r -> T B r -> T C r --@ unify the interesting arguments of a @Foo@.
--
--  * @fooG :: (...) => R C --@ guard for the constructor @Foo@.
--
--  * @fooP :: (...) => (A -> B -> T C a) -> T C a --@ pattern matching on @Foo@.
--
--  * @withFoo :: (..., Monad m) => (A -> B -> m a) -> C -> m a --@ application and pattern matching on @Foo@.
--
-- Here, R is short for a 'Rewrite', and 'T is short for 'Translate'.
--
-- An example of use is
--
-- > $(kureYourBoilerplate ''MyGeneric ''Id ''())
--
-- Which means @MyGeneric@ is my universal type, @Id@ is my monad, and @()@ is my monoid.

module Language.KURE.Boilerplate
        ( kureYourBoilerplate
        )

 where

import Debug.Trace

import Language.KURE.Types
import Language.KURE.Combinators

import Language.Haskell.TH

import Data.Char
import Data.Monoid
import Control.Monad
import System.Environment

-- | 'kureYourBoilerplate' generates a number of functions for each data-type mentioned in
-- our given Generic data-type, which we need to provide for KURE, as well as the
-- Walker instance.
--
-- The first argument is the name of the Generic data-structure, which you need to write by hand.
-- If you provide the name of a type synonym as the first argument, then KYB assumes that you are acting
-- over a single data-type, i.e. you generic is your AST type.
-- If you provide the name of a data-type  (the typical use-case), then this function generates
-- code for every conceptual sub-type of the provided data-type.
--
-- The second argument is the monad over which you will be parameterizing your rewrite rules,
-- and the third argument is the monoid over which you will be parameterizing.
--
kureYourBoilerplate :: Name -> Q [Dec]
kureYourBoilerplate gname = do
  debug <- runIO $ (do _k_debug <- getEnv "KURE_DEBUG"
                       return $ True) `catch` (\ _ -> return True)
  info <- reify gname

  runIO $ print info

  (api_tys) <- case info of
              TyConI (DataD _ _ _ cons _) -> do
                -- we look at *types* so that we can support more in the future.
                let (gcons,tys) = unzip [ (con,argTy) | (NormalC con [(_,argTy)]) <- cons ]
                when (length tys /= length cons) $ do
                        fail $ "Strange type inside Generic datatype: " ++ show gname
                mapM_ (pprintTermInstances gname) (zip gcons tys)
                return $ Left (gcons,tys)
              TyConI (TySynD _ [] singleTy) ->
                return $ Right singleTy-- no special generic instance needed
              _ -> fail $ "problem with generic type name " ++ show gname
  runIO $ print ("API_tys",api_tys)
  runIO $ putStrLn "---------------------------------"
  tyInfo <- case api_tys of
                        Left (gcons,tys) -> do tys' <- mapM resolveSynonym tys
                                               return $ Left (gcons,tys')
                        Right singleTy -> do singleTy' <- resolveSynonym singleTy
                                             return $ Right singleTy'
  runIO $ print ("tyNames",tyInfo)
  runIO $ putStrLn "---------------------------------"

  t <- newName "t"

  (decs,allR',allU') <-
        case tyInfo of
           Right ty -> do
                (d,ar,au) <- kureType debug [pprint ty]
                                      [ TySynInstD ''Generic [ty] (ConT gname)
                                      , FunD (mkName "select")
                                        [ Clause [VarP t] (NormalB (AppE (ConE 'Just) (VarE t))) []
                                        ]
                                      , FunD (mkName "inject")
                                        [ Clause [VarP t] (NormalB (VarE t)) []
                                        ]
                                      ] ty
                return ([d],[ar],[au])
           Left (cons,api_resolved_tys) -> do
                liftM unzip3 $ sequence [ kureType debug (map pprint api_resolved_tys)
                                                        [ TySynInstD ''Generic [ty] (ConT gname)
                                                        , FunD (mkName "select")
                                                          [ Clause [ConP con [VarP t]] (NormalB (AppE (ConE 'Just) (VarE t))) []
                                                          , Clause [WildP] (NormalB (ConE 'Nothing)) []
                                                          ]
                                                        , FunD (mkName "inject")
                                                          [ Clause [VarP t] (NormalB (AppE (ConE con) (VarE t))) []
                                                          ]
                                                        ] ty
                                        | (con,ty) <- zip cons api_resolved_tys
                                        ]

  rr <- newName "rr"

  -- Here, we find a way to promote from the generic type to specific type(s).
  let mkPromote prom nm _ = AppE (VarE prom) (AppE (VarE nm) (VarE rr))

  theOptGenericInstance <-
         case tyInfo of
           Left (_,api_resolved_tys) -> do
             let choice e1 e2 = InfixE (Just e1) (VarE '(<+)) (Just e2)
             let altsR = [ mkPromote 'promoteR nm ty
                         | (FunD nm _,ty) <- zip allR' api_resolved_tys
                         ]
             let altsU = [ mkPromote 'promoteU nm ty
                         | (FunD nm _,ty) <- zip allU' api_resolved_tys
                         ]
             return [ InstanceD []
                              (foldl AppT (ConT ''Term) [ConT gname])
                              [ TySynInstD ''Generic [ConT gname] (ConT gname)
                              , FunD (mkName "allR") [ Clause [VarP rr] (NormalB $ foldl1 choice altsR) allR']
                             , FunD (mkName "crushU") [ Clause [VarP rr] (NormalB $ foldl1 choice altsU) allU']
                              ]
                    ]

           _ -> return []

  let alldecs = concat decs ++ theOptGenericInstance
  when debug $ runIO $ do putStrLn $ pprint alldecs


  return $ alldecs

-- A type for which we want an API for a generic allR, etc.
kureType :: Bool -> [String] -> [Dec] -> Type -> Q ([Dec],Dec,Dec)
kureType debug tyNames tfs ty@(ConT nm)  = do
  info <- reify nm
  cons <- case info of
            TyConI (DataD _ _ _ cons _) -> return cons
            _ -> fail $ "strange info on name " ++ show nm ++ " : " ++ show info
  (decs,consNames,argTypess) <- liftM unzip3 $ sequence [ kureCons debug tyNames con | con <- cons ]
  rr <- newName "rr"
  let buildFn name suffix extract = FunD name
             [ Clause [VarP rr] (NormalB $ foldl1 choice alts) []]
          where
             choice e1 e2 = InfixE (Just e1) (VarE '(<+)) (Just e2)
             alts = [ foldl AppE (VarE (mkName $ consName ++ suffix))
                                 [ mkExtract tyNames extract rr ty'
                                 | ty' <- argTypes
                                 ]
                    | (consName,argTypes) <- zip consNames argTypess
                    ]
  let theInstance = InstanceD []
                              (foldl AppT (ConT ''Term) [ty])
                              ([ buildFn (mkName "allR")   "R" R
                              , buildFn (mkName "crushU") "U" U
                              ] ++ tfs)

  allR_nm <- newName "allR"
  allU_nm <- newName "allU"

  return ( concat decs ++ [theInstance]
         , buildFn allR_nm "R" R
         , buildFn allU_nm "U" U
         )

-- A bit of a hack for now
kureType _debug tyNames tfs ty = do
  runIO $ print ("kureType",tyNames,tfs,ty)

  -- For other types, we do not generate the G,U, etc, functions, but do define
  -- the *all* instance, which works directly over the type.
  rr <- newName "rr"
  tup <- newName "x"
  let buildFn name _ extract = FunD name
            [ Clause [VarP rr] (NormalB
                                        $ AppE (VarE 'translate)
                                        $ LamE [VarP tup]
                                        $ AppE ( AppE (VarE 'apply)
                                                      (AppE (VarE extract) (VarE rr))
                                                )
                                                (SigE (VarE tup) ty)
                                ) []]
  let theInstance = InstanceD []
                              (foldl AppT (ConT ''Term) [ty])
                              ([ buildFn (mkName "allR")   "R" 'extractR
                              , buildFn (mkName "crushU") "U" 'extractU
                              ] ++ tfs)

  allR_nm <- newName "allR"
  allU_nm <- newName "allU"

  return ( [theInstance]
         , buildFn allR_nm "R" 'extractR
         , buildFn allU_nm "U" 'extractU
         )

kureCons :: Bool -> [String] -> Con -> Q ([Dec],String,[Type])
kureCons _debug tyNames (NormalC consName args)  = do
        runIO $ print ("kureCons", consName, args)
        let guardName = mkName (combName consName ++ "G")
        v <- newName "v"
        let guardExpr = AppE (VarE 'acceptR)  (LamE [VarP v]
              (CaseE (VarE v)
                [ Match (RecP consName [])
                        (NormalB (ConE 'True)) []
                , Match WildP (NormalB (ConE 'False)) []
                ]))
        let guardDef = ValD (VarP guardName) (NormalB guardExpr) []

        let withName = mkName ("with" ++ nameBase consName)
        (f:vs) <- mapM newName ("f": ["v" | _ <- args])
        let withDef = FunD withName
               [ Clause [VarP f,ConP consName (map VarP vs)] (NormalB (foldl AppE (VarE f) (map VarE vs))) []

               , Clause [WildP,WildP] (NormalB (AppE (VarE 'fail) (LitE (StringL (show withName ++ " failed"))))) []

               ]

        let nameR = mkName (combName consName ++ "R")
        let isInteresting ty@(VarT {}) _ = error $ "found " ++ pprint ty ++ " as argument to " ++ show consName
            isInteresting ty [] | pprint ty `elem` tyNames
                                          = True
            isInteresting    (ConT _ ) [] = False       -- the above case caught this
            isInteresting    (ConT nm) [inner_ty]
                | nm == ''[] = isInteresting inner_ty []
                | nm == ''Maybe = isInteresting inner_ty []
            isInteresting (ConT nm) tys
                | length tys >= 2 && nm == tupleTypeName (length tys) = or [ isInteresting ty [] | ty <- tys ]
            isInteresting (AppT e1 e2) es = isInteresting e1 (e2:es)
            isInteresting ty         _ = error $ "unsupported type " ++ pprint ty ++ " as argument to " ++ show consName

        resolvedArgsTypes <- mapM resolveSynonym argsTypes
        -- This denotes if the 'R' combinator and 'U' combinator will have an explicitly called out argument.
        let interestingConsArgs = [ isInteresting ty [] | ty <- resolvedArgsTypes ]

        rrs <- mapM newName [ "rr" | True <- interestingConsArgs ]
        es  <- mapM newName ["e"  | _ <- args ]
        es' <- sequence [ if interesting
                          then liftM Just $ newName "e'"
                          else return $ Nothing
                        | interesting <- interestingConsArgs ]

        let es'' = [ case opt_e' of
                       Just e' -> e'
                       _ -> e
                   | (e,opt_e') <- zip es es'
                   ]

                -- eek, this wiring is undocumented.
        let es'_rrs_es = [ (e',rr,e)
                         | (rr,(e,e')) <- zip rrs
                                [  (e,e') | (e,Just e') <- zip es es' ]
                         ]

        let nameRExpr =      (AppE (VarE 'rewrite)
                             (AppE (VarE withName)
                                   (LamE (map VarP es)
                                   (DoE (  [ BindS (VarP e')
                                                   (foldl AppE (VarE 'apply) (map VarE [rr,e]))
                                           | (e',rr,e) <- es'_rrs_es
                                           ]
                                       ++ [ NoBindS $
                                            AppE (VarE 'return)
                                               $ foldl AppE (ConE consName) (map VarE es'')
                                          ])))))

        let nameRDef = FunD nameR [ Clause (map VarP rrs) (NormalB nameRExpr) []]

        let nameU = mkName (combName consName ++ "U")

        let nameUExpr = AppE (VarE 'translate)
                             (AppE (VarE withName)
                                   (LamE (map VarP es)
                                   (DoE (  [ BindS (VarP e')
                                                   (foldl AppE (VarE 'apply) (map VarE [rr,e]))
                                           | (e',rr,e) <- es'_rrs_es
                                           ]
                                       ++ [ NoBindS $
                                            AppE (VarE 'return)
                                               $ AppE (VarE 'mconcat) (ListE (map VarE [ e' | Just e' <- es']))
                                          ]))))

        let nameUDef = FunD nameU [ Clause (map VarP rrs) (NormalB nameUExpr) []]

        let nameP = mkName (combName consName ++ "P")
        the_e <- newName "the_e"
        let namePExpr =      (AppE (VarE 'translate)
                             (LamE [VarP the_e]
                               (AppE (AppE (VarE withName)
                                     (LamE (map VarP es)
                                           (AppE (AppE (VarE 'apply)
                                                       (foldl AppE (VarE f) (map VarE es))
                                                  )
                                                  (VarE the_e)
                                           )
                                       )
                                    )
                                (VarE the_e))
                           ))

        let namePDef = FunD nameP [ Clause [VarP f] (NormalB namePExpr) []]

        return ( [guardDef,withDef,nameRDef,nameUDef,namePDef]
               , combName consName
               , [ ty | (True,ty) <- zip interestingConsArgs resolvedArgsTypes ]
               )
   where
        argsTypes = map snd args
kureCons _ _tyNames other  = error $ "Unsupported constructor : " ++ show other

mkExtract :: [String] -> ResultStyle -> Name -> Type -> Exp
mkExtract t e r ty | trace ("mkExtract: " ++ show (t,e,r,ty)) False = undefined
mkExtract tyNames extract rr ty | pprint ty `elem` tyNames
                                              = AppE (VarE $ theExtract extract) (VarE rr)
mkExtract tyNames extract rr (AppT e1 e2)     = mkExtract' tyNames extract rr e1 [e2]
mkExtract _       extract _  _                = VarE $ theEmpty extract

mkExtract' :: [String] -> ResultStyle -> Name -> Type -> [Type] -> Exp
mkExtract' tyNames extract rr (AppT e1 e2) es   = mkExtract' tyNames extract rr e1 (e2:es)
mkExtract' tyNames extract rr (ConT con) [t1,t2]
        | con == tupleTypeName 2                =  AppE (AppE (VarE $ theTuple2 extract)
                                                              (mkExtract tyNames extract rr t1)
                                                        )     (mkExtract tyNames extract rr t2)

mkExtract' tyNames extract rr (ConT con) [t1]
        | con == ''[]                           =  AppE (VarE $ theList extract)
                                                        (mkExtract tyNames extract rr t1)
mkExtract' tyNames extract rr (ConT con) [t1]
        | con == ''Maybe                        =  AppE (VarE $ theMaybe extract)
                                                        (mkExtract tyNames extract rr t1)
mkExtract' _       _       _  ty         _      = error $ "mkExtract' failed for " ++ pprint ty


data ResultStyle = R | U
    deriving (Eq, Show)

-- Perhaps a fixed table?

theExtract :: ResultStyle -> Name
theExtract R = 'extractR
theExtract U = 'extractU

theTuple2 :: ResultStyle -> Name
theTuple2 R = 'tuple2R
theTuple2 U = 'tuple2U

theList :: ResultStyle -> Name
theList R = 'listR
theList U = 'listU

theMaybe :: ResultStyle -> Name
theMaybe R = 'maybeR
theMaybe U = 'maybeU

theEmpty :: ResultStyle -> Name
theEmpty R = 'idR
theEmpty U = 'emptyT

combName :: Name -> String
combName nm = case nameBase nm of
                (t:ts) -> toLower t : ts
                [] -> ""


resolveSynonym:: Type -> Q Type
resolveSynonym ty@(ConT con) = do
  info <- reify con
--  runIO $ print info
  case info of
    TyConI (DataD _ _ _ _ _)      -> return $ ty
    TyConI (NewtypeD _ _ _ _ _)   -> return $ ty
    TyConI (TySynD _ [] ty2)      -> resolveSynonym ty2
    _ -> fail $ "unable to resolve synonym - unknown info inside " ++ show con ++ " ( " ++ show info ++ ")"

resolveSynonym (AppT e1 e2) = do
        e1' <- resolveSynonym e1
        e2' <- resolveSynonym e2
        return $ AppT e1' e2'

resolveSynonym other = fail $ "resolveSynonym problem : " ++ show other

pprintTermInstances :: Name -> (Name,Type) -> Q ()
pprintTermInstances gnm (nm,ty) =
  runIO $ do
        putStrLn $ ""
        putStrLn $ "--------------------------------------------------"
        putStrLn $ "instance Term " ++ pprint ty ++ " where"
        putStrLn $ "  type Generic " ++ pprint ty ++ " = " ++ nameBase gnm
        putStrLn $ "  select (" ++ nameBase nm ++ " e) = Just e"
        putStrLn $ "  select _        = Nothing"
        putStrLn $ "  inject          = " ++ nameBase nm
        putStrLn $ "--------------------------------------------------"
        putStrLn $ ""
