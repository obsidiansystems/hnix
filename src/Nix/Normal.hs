{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nix.Normal where

import           Control.Monad
import           Control.Monad.Free
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Set (Set)
import qualified Data.Set as Set
import           Nix.Atoms
import           Nix.Effects
import           Nix.Frames
-- import           Nix.Pretty
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

newtype NormalLoop m = NormalLoop (NValue m)
    deriving Show

instance Typeable m => Exception (NormalLoop m)

normalFormBy
    :: forall e m. (Framed e m, MonadVar m, Typeable m)
    => (forall r. NThunk m -> (NValue m -> m r) -> m r)
    -> Set (NValue m)
    -> NValue m
    -> m (NValueNF m)
normalFormBy k vs v = do
    -- doc <- prettyNValue v
    -- traceM $ show n ++ ": normalFormBy: " ++ show doc
    if Set.member v vs
    then return $ Pure v
    else let vs' = Set.insert v vs in case v of
        NVConstant a     -> return $ Free $ NVConstantF a
        NVStr t s        -> return $ Free $ NVStrF t s
        NVList l         ->
            fmap (Free . NVListF) $ forM (zip [0..] l) $ \(i :: Int, t) -> do
                traceM $ "normalFormBy: List[" ++ show i ++ "]"
                t `k` normalFormBy k vs'
        NVSet s p        ->
            fmap (Free . flip NVSetF p) $ sequence $ flip M.mapWithKey s $ \ky t -> do
                traceM $ "normalFormBy: Set{" ++ show ky ++ "}"
                t `k` normalFormBy k vs'
        NVClosure p f    -> return $ Free $ NVClosureF p f
        NVPath fp        -> return $ Free $ NVPathF fp
        NVBuiltin name f -> return $ Free $ NVBuiltinF name f
        _ -> error "Pattern synonyms mask complete matches"

normalForm :: (Framed e m, MonadVar m, Typeable m,
              MonadThunk (NValue m) (NThunk m) m)
           => NValue m -> m (NValueNF m)
normalForm = normalFormBy force Set.empty

embed :: forall m. (MonadThunk (NValue m) (NThunk m) m)
      => NValueNF m -> m (NValue m)
embed (Pure v) = return v
embed (Free x) = case x of
    NVConstantF a  -> return $ nvConstant a
    NVStrF t s     -> return $ nvStr t s
    NVListF l      -> nvList       . fmap (value @_ @_ @m) <$> traverse embed l
    NVSetF s p     -> flip nvSet p . fmap (value @_ @_ @m) <$> traverse embed s
    NVClosureF p f -> return $ nvClosure p f
    NVPathF fp     -> return $ nvPath fp
    NVBuiltinF n f -> return $ nvBuiltin n f

valueText :: forall e m. (Framed e m, MonadEffects m, Typeable m)
          => Bool -> NValueNF m -> m (Text, DList Text)
valueText addPathsToStore = iter phi . check
  where
    check :: NValueNF m -> Free (NValueF m) (m (Text, DList Text))
    check = fmap (const $ pure ("<CYCLE>", mempty))

    phi :: NValueF m (m (Text, DList Text)) -> m (Text, DList Text)
    phi (NVConstantF a) = pure (atomText a, mempty)
    phi (NVStrF t c)    = pure (t, c)
    phi v@(NVListF _)   = coercionFailed v
    phi v@(NVSetF s _)
      | Just asString <- M.lookup "__asString" s = asString
      | otherwise = coercionFailed v
    phi v@NVClosureF {} = coercionFailed v
    phi (NVPathF originalPath)
        | addPathsToStore = do
            storePath <- addPath originalPath
            pure (Text.pack $ unStorePath storePath, mempty)
        | otherwise = pure (Text.pack originalPath, mempty)
    phi v@(NVBuiltinF _ _) = coercionFailed v

    coercionFailed v =
        throwError $ Coercion @m (valueType v) TString

valueTextNoContext :: (Framed e m, MonadEffects m, Typeable m)
                   => Bool -> NValueNF m -> m Text
valueTextNoContext addPathsToStore = fmap fst . valueText addPathsToStore
