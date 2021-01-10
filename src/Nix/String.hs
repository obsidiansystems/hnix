{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nix.String
  ( NixString
  , getContext
  , makeNixString
  , StringContext(..)
  , ContextFlavor(..)
  , NixLikeContext(..)
  , NixLikeContextValue(..)
  , toNixLikeContext
  , fromNixLikeContext
  , stringHasContext
  , intercalateNixString
  , getStringNoContext
  , stringIgnoreContext
  , makeNixStringWithoutContext
  , makeNixStringWithSingletonContext
  , modifyNixContents
  , WithStringContext
  , WithStringContextT(..)
  , extractNixString
  , addStringContext
  , addSingletonStringContext
  , runWithStringContextT
  , runWithStringContextT'
  , runWithStringContext
  , runWithStringContext'
  )
where

import           Control.Monad.Writer
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashSet                  as S
import           Data.Hashable
import           Data.Semigroup
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics

-- * Types

-- ** Context

-- | A 'StringContext' ...
data StringContext =
  StringContext { scPath :: !Text
                , scFlavor :: !ContextFlavor
                } deriving (Eq, Ord, Show, Generic)

instance Hashable StringContext

-- | A 'ContextFlavor' describes the sum of possible derivations for string contexts
data ContextFlavor =
    DirectPath
  | AllOutputs
  | DerivationOutput !Text
  deriving (Show, Eq, Ord, Generic)

instance Hashable ContextFlavor

newtype NixLikeContext = NixLikeContext
  { getNixLikeContext :: M.HashMap Text NixLikeContextValue
  } deriving (Eq, Ord, Show, Generic)

data NixLikeContextValue = NixLikeContextValue
  { nlcvPath :: !Bool
  , nlcvAllOutputs :: !Bool
  , nlcvOutputs :: ![Text]
  } deriving (Show, Eq, Ord, Generic)

instance Semigroup NixLikeContextValue where
  a <> b = NixLikeContextValue
    { nlcvPath       = nlcvPath a || nlcvPath b
    , nlcvAllOutputs = nlcvAllOutputs a || nlcvAllOutputs b
    , nlcvOutputs    = nlcvOutputs a <> nlcvOutputs b
    }

instance Monoid NixLikeContextValue where
  mempty = NixLikeContextValue False False []


-- ** StringContext accumulator

-- | A monad for accumulating string context while producing a result string.
newtype WithStringContextT m a = WithStringContextT (WriterT (S.HashSet StringContext) m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter (S.HashSet StringContext))

type WithStringContext = WithStringContextT Identity


-- ** NixString

data NixString = NixString
  { nsContents :: !Text
  , nsContext :: !(S.HashSet StringContext)
  } deriving (Eq, Ord, Show, Generic)

instance Semigroup NixString where
  NixString s1 t1 <> NixString s2 t2 = NixString (s1 <> s2) (t1 <> t2)
  --NOTE: As of version 1.2.4.0, text does not define sconcat, which means we
  --need to use its mconcat to get good performance
  sconcat = mconcat . toList

instance Monoid NixString where
  mempty = NixString mempty mempty
  -- Important for performance
  mconcat strs = NixString (mconcat $ nsContents <$> strs) (mconcat $ nsContext <$> strs)

instance Hashable NixString


-- * Functions

-- ** Makers

-- | Constructs NixString without a context
makeNixStringWithoutContext :: Text -> NixString
makeNixStringWithoutContext = flip NixString mempty

-- | Create NixString using a singleton context
makeNixStringWithSingletonContext
  :: Text -> StringContext -> NixString
makeNixStringWithSingletonContext s c = NixString s (S.singleton c)

-- | Create NixString from a Text and context
makeNixString :: Text -> S.HashSet StringContext -> NixString
makeNixString = NixString


-- ** Checkers

-- | Returns True if the NixString has an associated context
stringHasContext :: NixString -> Bool
stringHasContext (NixString _ c) = not (null c)


-- ** Getters

getContext :: NixString -> S.HashSet StringContext
getContext = nsContext

fromNixLikeContext :: NixLikeContext -> S.HashSet StringContext
fromNixLikeContext =
  S.fromList . join . map toStringContexts . M.toList . getNixLikeContext

-- | Extract the string contents from a NixString that has no context
getStringNoContext :: NixString -> Maybe Text
getStringNoContext (NixString s c) | null c    = Just s
                                             | otherwise = Nothing

-- | Extract the string contents from a NixString even if the NixString has an associated context
stringIgnoreContext :: NixString -> Text
stringIgnoreContext (NixString s _) = s

-- | Get the contents of a 'NixString' and write its context into the resulting set.
extractNixString :: Monad m => NixString -> WithStringContextT m Text
extractNixString (NixString s c) = WithStringContextT $ tell c >> pure s


-- ** Setters

toStringContexts :: (Text, NixLikeContextValue) -> [StringContext]
toStringContexts (path, nlcv) = case nlcv of
  NixLikeContextValue True _ _ -> StringContext path DirectPath
    : toStringContexts (path, nlcv { nlcvPath = False })
  NixLikeContextValue _ True _ -> StringContext path AllOutputs
    : toStringContexts (path, nlcv { nlcvAllOutputs = False })
  NixLikeContextValue _ _ ls | not (null ls) ->
    map (StringContext path . DerivationOutput) ls
  _ -> []

toNixLikeContextValue :: StringContext -> (Text, NixLikeContextValue)
toNixLikeContextValue sc = (,) (scPath sc) $ case scFlavor sc of
  DirectPath         -> NixLikeContextValue True False []
  AllOutputs         -> NixLikeContextValue False True []
  DerivationOutput t -> NixLikeContextValue False False [t]

toNixLikeContext :: S.HashSet StringContext -> NixLikeContext
toNixLikeContext stringContext = NixLikeContext
  $ S.foldr go mempty stringContext
 where
  go sc hm =
    let (t, nlcv) = toNixLikeContextValue sc in M.insertWith (<>) t nlcv hm

-- | Add 'StringContext's into the resulting set.
addStringContext
  :: Monad m => S.HashSet StringContext -> WithStringContextT m ()
addStringContext = WithStringContextT . tell

-- | Add a 'StringContext' into the resulting set.
addSingletonStringContext :: Monad m => StringContext -> WithStringContextT m ()
addSingletonStringContext = WithStringContextT . tell . S.singleton

-- | Run an action producing a string with a context and put those into a 'NixString'.
runWithStringContextT :: Monad m => WithStringContextT m Text -> m NixString
runWithStringContextT (WithStringContextT m) =
  uncurry NixString <$> runWriterT m

-- | Run an action producing a string with a context and put those into a 'NixString'.
runWithStringContext :: WithStringContextT Identity Text -> NixString
runWithStringContext = runIdentity . runWithStringContextT


-- ** Modifiers

-- | Modify the string part of the NixString, leaving the context unchanged
modifyNixContents :: (Text -> Text) -> NixString -> NixString
modifyNixContents f (NixString s c) = NixString (f s) c

-- | Run an action that manipulates nix strings, and collect the contexts encountered.
-- Warning: this may be unsafe, depending on how you handle the resulting context list.
runWithStringContextT' :: Monad m => WithStringContextT m a -> m (a, S.HashSet StringContext)
runWithStringContextT' (WithStringContextT m) = runWriterT m

-- | Run an action that manipulates nix strings, and collect the contexts encountered.
-- Warning: this may be unsafe, depending on how you handle the resulting context list.
runWithStringContext' :: WithStringContextT Identity a -> (a, S.HashSet StringContext)
runWithStringContext' = runIdentity . runWithStringContextT'

-- | Combine NixStrings with a separator
intercalateNixString :: NixString -> [NixString] -> NixString
intercalateNixString _   []   = mempty
intercalateNixString _   [ns] = ns
intercalateNixString sep nss  = NixString contents ctx
 where
  contents = Text.intercalate (nsContents sep) (map nsContents nss)
  ctx      = S.unions (nsContext sep : map nsContext nss)

