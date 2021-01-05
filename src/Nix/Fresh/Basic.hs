{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Fresh.Basic where

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail ( MonadFail )
#endif
import           Control.Monad.Reader
import           Nix.Effects
import           Nix.Render
import           Nix.Fresh
import           Nix.Fresh.Stable
import           Nix.Value

type StdIdT = FreshStableIdT
