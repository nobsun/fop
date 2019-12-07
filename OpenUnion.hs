{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module OpenUnion where

import           Prelude hiding ((.), id)
import qualified Prelude
import           Control.Category
import           Data.Extensible
import           Data.Kind
import           Data.Type.Equality


newtype f :~> g = Nat { unNat :: forall i. f i -> g i }
instance Category (:~>) where
  id = Nat Prelude.id
  Nat f . Nat g = Nat (f Prelude.. g)

class HFunctor (f :: (k -> Type) -> (k -> Type)) where
  hfmap :: (a :~> b) -> (f a :~> f b)

hfmapLack :: HFunctor f => (a :~> b) -> f a i -> f b i
hfmapLack f = unNat (hfmap f)

data HSum f g r i = HInL (f r i) | HInR (g r i)

hsum :: f r :~> a -> g r :~> a -> HSum f g r :~> a
hsum (Nat lf) (Nat rf) = Nat \case
  HInL x -> lf x
  HInR x -> rf x


instance (HFunctor f, HFunctor g) => HFunctor (HSum f g) where
  hfmap f = hsum (Nat HInL . hfmap f) (Nat HInR . hfmap f)

data HUnion fs r i where
  HUnion :: Membership fs f -> f r i -> HUnion fs r i

injectU :: Member fs f => f r :~> HUnion fs r
injectU = Nat \x -> HUnion membership x

{--
-- couldn't compile
decomp :: HUnion (f ': fs) r :~> HSum (HUnion fs) f r
decomp = Nat \case
  HUnion n x -> leadership n (\Refl -> HInR x) (\m -> HInL $ HUnion m x)

comp :: HSum (HUnion fs) f r :~> HUnion (f ': fs) r
comp = hsum weaken $ Nat \x -> HUnion here x

weaken :: HUnion fs r :~> HUnion (f ': fs) r
weaken = Nat \case
  HUnion n x -> HUnion (navNext n) x

absurdU :: HUnion '[] r :~> a
absurdU = Nat \case
  HUnion n _ -> impossibleMembership n

instance HFunctor (HUnion '[]) where
  hfmap _ = absurdU

instance (HFunctor f, HFunctor (HUnion fs)) => HFunctor (HUnion (f ': fs)) where
  hfmap f = comp . hfmap f . decomp
--}
