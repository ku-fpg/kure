{-# LANGUAGE TupleSections #-}

-- |
-- Module: Language.KURE.Translate
-- Copyright: (c) 2006-2012 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: alpha
-- Portability: ghc
--
-- This module defines the main KURE types: 'Translate' and 'Rewrite'.
-- It also contains various combinators that operate over these types. The convention is that
-- 'Translate' based combinators end with @T@, and 'Rewrite' based combinators end with @R@. Of course,
-- because 'Rewrite' is a type synonymm of 'Translate', the 'Rewrite' functions also operate with on 'Translate',
-- and the 'Translate' functions operate with 'Rewrite'.

module Language.KURE.Translate
       (  -- | Translations
          Translate
        , translate
        , apply
        , (<+)
        , (>->)
        , contextT
        , exposeT
        , liftMT
        , liftT
        , constMT
        , readerT
          -- | Rewrites
        , Rewrite
        , rewrite
        , idR
        , acceptR
        , tryR
        , attemptR
        , changedR
        , repeatR
        , (>+>)
        , orR
        , andR
          -- | Prelude combinators
        , tuple2R
        , listR
        , maybeR
        , tuple2T
        , listT
        , maybeT
          -- | Lenses
        , Lens
        , lens
        , idL
        , tryL
        , composeL
        , sequenceL
        , focusR
        , focusT
          -- | Combinators that generalise to an arbitrary 'Monad' or 'Alternative'
        , guardFail
        , condM
        , whenM
        , notM
        , mconcatA
        , memptyA
        , tryA
        , mtryA
        , attemptA
        , testA

) where

import Prelude hiding (id, (.))
import Data.Monoid
import Data.Maybe (isJust)
import Data.Traversable (sequenceA)
import Control.Applicative
import Control.Monad
import Control.Category
import Control.Arrow

infixl 3 <+, >->, >+>

------------------------------------------------------------------------------------------

-- | 'Translate' is a translation or strategy that translates from a value in a context to a monadic value.
data Translate c m a b = Translate {apply :: c -> a -> m b}

-- | A 'Rewrite' is a 'Translate' that shares the same source and target type.
type Rewrite c m a = Translate c m a a

-- | 'translate' is the primitive  way of building a 'Translate'.
translate :: (c -> a -> m b) -> Translate c m a b
translate = Translate

-- | 'rewrite' is the primitive way of building a 'Rewrite'.
rewrite :: (c -> a -> m a) -> Rewrite c m a
rewrite = translate

------------------------------------------------------------------------------------------

-- | identity rewrite.
idR :: Applicative m => Rewrite c m a
idR = rewrite (\ _ -> pure)

-- | extract the current context.
contextT :: Applicative m => Translate c m a c
contextT = translate (\ c _ -> pure c)

-- | expose the current context and value.
exposeT :: Applicative m => Translate c m a (c,a)
exposeT = translate (\ c a -> pure (c,a))

-- | lift an effectful function into a 'Translate'.
liftMT :: (a -> m b) -> Translate c m a b
liftMT = translate . const

-- | lift a pure function into a 'Translate'.
liftT :: Applicative m => (a -> b) -> Translate c m a b
liftT f = liftMT (pure . f)

-- | 'constMT' lifts an effectful computation into a constant 'Translate'.
constMT :: m b -> Translate c m a b
constMT = liftMT . const

-- | like a catch, '<+' does the first 'Translate', and if it fails, then does the second 'Translate'.
(<+) :: Alternative m => Translate c m a b -> Translate c m a b -> Translate c m a b
t1 <+ t2 = translate $ \ c a -> apply t1 c a <|> apply t2 c a

-- | sequencing translates.
(>->) :: Monad m => Translate c m a b -> Translate c m b d -> Translate c m a d
t1 >-> t2 = translate $ \ c -> apply t1 c >=> apply t2 c

------------------------------------------------------------------------------------------

instance Functor m => Functor (Translate c m a) where

-- fmap :: (b -> d) -> Translate c m a b -> Translate c m a d
   fmap f t = translate (\ c -> fmap f . apply t c)

instance Applicative m => Applicative (Translate c m a) where

-- pure :: b -> Translate c m a b
   pure = constMT . pure

-- (<*>) :: Translate c m a (b -> d) -> Translate c m a b -> Translate c m a d
   tf <*> tb = translate (\ c a -> apply tf c a <*> apply tb c a)

instance Alternative m => Alternative (Translate c m a) where

-- empty :: Translate c m a b
   empty = constMT empty

-- (<|>) :: Translate c m a b -> Translate c m a b -> Translate c m a b
   (<|>) = (<+)

instance Monad m => Monad (Translate c m a) where

-- return :: b -> Translate c m a b
   return = constMT . return

-- (>>=) :: Translate c m a b -> (b -> Translate c m a d) -> Translate c m a d
   tb >>= f = translate $ \ c a -> do b <- apply tb c a
                                      apply (f b) c a

-- fail :: String -> Translate c m a b
   fail = constMT . fail

instance MonadPlus m => MonadPlus (Translate c m a) where

-- mzero :: Translate c m a b
   mzero = constMT mzero

-- mplus :: Translate c m a b -> Translate c m a b -> Translate c m a b
   mplus t1 t2 = translate $ \ c a -> apply t1 c a `mplus` apply t2 c a

instance (Applicative m, Monad m) => Category (Translate c m) where

--  id :: Translate c m a a
    id = idR

--  (.) :: Translate c m b d -> Translate c m a b -> Translate c m a d
    t2 . t1 = t1 >-> t2

instance (Applicative m, Monad m) => Arrow (Translate c m) where

-- arr :: (a -> b) -> Translate c m a b
   arr = liftT

-- first :: (a -> b) -> Translate c m (a,z) (b,z)
   first t = translate $ \ c (a,z) -> liftA (\b -> (b,z)) (apply t c a)

------------------------------------------------------------------------------------------

-- | look at the argument for the translation before choosing which 'Translate' to perform.
readerT :: (a -> Translate c m a b) -> Translate c m a b
readerT f = translate $ \ c a -> apply (f a) c a

-- | look at the argument to a 'Rewrite', and choose to be either a failure or trivial success.
acceptR :: Alternative m => (a -> Bool) -> Rewrite c m a
acceptR p = rewrite $ \ _ a -> if p a then pure a else empty

-- | catch a failing 'Rewrite', making it into an identity.
tryR :: Alternative m => Rewrite c m a -> Rewrite c m a
tryR r = r <+ idR

-- | catch a failing 'Rewrite', making it succeed with a Boolean flag.
--   Useful when defining @anyR@ instances.
attemptR :: Alternative m => Rewrite c m a -> Translate c m a (Bool,a)
attemptR r = fmap (True,) r <+ fmap (False,) idR

-- | makes a 'Rewrite' fail if the value result equals the initial value
changedR :: (Alternative m, Monad m, Eq a) => Rewrite c m a -> Rewrite c m a
changedR r = readerT (\ a -> r >-> acceptR (/=a))

-- | repeat a 'Rewrite' until it fails, then return the result before the failure.
repeatR :: (Alternative m, Monad m) => Rewrite c m a -> Rewrite c m a
repeatR r = tryR (r >-> repeatR r)

-- | attempts two 'Rewrite's in sequence, succeeding if one or both succeed.
(>+>) :: (Alternative m, Monad m) => Rewrite c m a -> Rewrite c m a -> Rewrite c m a
r1 >+> r2 = rewrite $ \ c a -> apply (attemptA r1) c a >>= maybe (apply r2 c a) (apply (tryR r2) c)

-- | attempt a list of 'Rewrite's in sequence, succeeding if at least one succeeds.
orR :: (Alternative m, Monad m) => [Rewrite c m a] -> Rewrite c m a
orR = foldl (>+>) empty

-- | perform a list of 'Rewrite's in sequence, succeeding if they all succeed.
andR :: (Applicative m, Monad m) => [Rewrite c m a] -> Rewrite c m a
andR = foldl (>->) idR

------------------------------------------------------------------------------------------

-- | Equivalent to (***), but with less class constraints
tuple2R :: Applicative m => Rewrite c m a -> Rewrite c m b -> Rewrite c m (a,b)
tuple2R r1 r2 = rewrite $ \ c (a,b) -> (,) <$> apply r1 c a <*> apply r2 c b

listR :: Applicative m => Rewrite c m a -> Rewrite c m [a]
listR r = rewrite $ \ c -> sequenceA . map (apply r c)

maybeR :: Applicative m => Rewrite c m a -> Rewrite c m (Maybe a)
maybeR r = rewrite $ \ c -> maybe (pure Nothing) (fmap Just . apply r c)

tuple2T :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m b r -> Translate c m (a,b) r
tuple2T t1 t2 = translate $ \ c (a,b) -> mappend <$> apply t1 c a <*> apply t2 c b

listT :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m [a] r
listT t = translate $ \ c -> fmap mconcat . sequenceA . map (apply t c)

maybeT :: (Applicative m, Monoid r) => Translate c m a r -> Translate c m (Maybe a) r
maybeT t = translate $ \ c -> maybe (pure mempty) (apply t c)

------------------------------------------------------------------------------------------

-- | A 'Lens' is a way to focus in on a particular point in a structure
type Lens c m a b = Translate c m a ((c,b), (b -> m a))

-- | 'lens' is the primitive way of building a 'Lens'.
lens :: (c -> a -> m ((c,b), (b -> m a))) -> Lens c m a b
lens = translate

-- | identity 'Lens'.
idL :: Applicative m => Lens c m a a
idL = lens $ \ c a -> pure ((c,a), pure)

-- | catch a failing endo'Lens', making it into an identity.
tryL :: Alternative m => Lens c m a a -> Lens c m a a
tryL l = l <+ idL

-- | composition of 'Lens's.
composeL :: Monad m => Lens c m a b -> Lens c m b d -> Lens c m a d
composeL l1 l2 = lens $ \ ca a -> do ((cb,b),kb) <- apply l1 ca a
                                     ((cd,d),kd) <- apply l2 cb b
                                     return ((cd,d),kd >=> kb)

-- | sequence a list of endo'Lens's.
sequenceL :: (Applicative m, Monad m) => [Lens c m a a] -> Lens c m a a
sequenceL = foldr composeL idL

-- | apply a 'Rewrite' at a point specified by a 'Lens'.
focusR :: Monad m => Lens c m a b -> Rewrite c m b -> Rewrite c m a
focusR l r = rewrite $ \ c a -> do ((cb,b),kb) <- apply l c a
                                   apply r cb b >>= kb

-- | apply a 'Translate' at a point specified by a 'Lens'.
focusT :: Monad m => Lens c m a b -> Translate c m b d -> Translate c m a d
focusT l t = translate $ \ c a -> do ((cb,b),_) <- apply l c a
                                     apply t cb b

------------------------------------------------------------------------------------------

-- | similar to 'guard', but using 'fail' rather than 'mzero'.
guardFail ::  Monad m => Bool -> String -> m ()
guardFail b msg = if b then return () else fail msg

-- | if-then-else lifted over a 'Monad'.
condM ::  Monad m => m Bool -> m a -> m a -> m a
condM mb m1 m2  = do b <- mb
                     if b then m1 else m2

-- | if-then lifted over a 'Monad'.
whenM ::  (Alternative m, Monad m) => m Bool -> m a -> m a
whenM mb ma = condM mb ma empty

------------------------------------------------------------------------------------------

-- | performs a list of 'Applicative's in order, then combines their result in a 'Monoid'.
mconcatA :: (Applicative m , Monoid a) => [m a] -> m a
mconcatA = liftA mconcat . sequenceA

-- | 'memptyA' always succeeds with 'mempty'
memptyA :: (Applicative m, Monoid a) => m a
memptyA = pure mempty

------------------------------------------------------------------------------------------

-- | catch a failing 'Alternative', making it succeed with a constant value.
tryA :: Alternative m => a -> m a -> m a
tryA a ma = ma <|> pure a

-- | catch a failing 'Alternative', making it succeed with 'mempty'.
mtryA :: (Alternative m, Monoid a) => m a -> m a
mtryA = tryA mempty

-- | catch a failing 'Alternative', making it succeed with 'Nothing'.
attemptA :: Alternative m => m a -> m (Maybe a)
attemptA ma = tryA Nothing (Just <$> ma)

-- | determine if an 'Alternative' succeeds.
testA :: Alternative m => m a -> m Bool
testA ma = isJust <$> attemptA ma

-- | 'notT' fails if the 'Alternative' succeeds, and succeeds with @()@ if it fails.
notM :: (Alternative m, Monad m) => m a -> m ()
notM ma = attemptA ma >>= maybe (pure ()) (const empty)

------------------------------------------------------------------------------------------
