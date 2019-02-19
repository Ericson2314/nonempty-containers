{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      : Data.Containers.Total
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = "Total" Container Newtypes.
--
-- Provides the newtypes 'Total' and 'NETotal', which abstracts over different
-- types which have a "non-empty" variant.
--
-- Used to convert between and in between possibly-empty and non-empty
-- types.  Instances are provided for all modules in this package, as well
-- as for 'NonEmpty' in /base/.
module Data.Containers.Total (
      Total(..)
    , NETotal(..)
    ) where

import           Data.Containers.NonEmpty

type family NonEmptyContainee s where
  NonEmptyContainee (Maybe s) = s
  NonEmptyContainee s = NE s

newtype Total s a = Total { unTotal :: s (NonEmptyContainee a) }

newtype NETotal s a = NETotal { unNETotal :: NE (s (NonEmptyContainee a)) }

instance HasNonEmpty (s (NonEmptyContainee a)) => HasNonEmpty (Total s a) where
    type NE (Total s a) = NETotal s a
    nonEmpty         = fmap NETotal . nonEmpty . unTotal
    fromNonEmpty     = fromNonEmpty
    withNonEmpty d f = withNonEmpty d (f . NETotal) . unTotal
    empty            = Total empty
    isEmpty          = isEmpty . unTotal
    unsafeToNonEmpty = NETotal . unsafeToNonEmpty . unTotal

