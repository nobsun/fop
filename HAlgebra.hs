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
module HAlgebra where

import           Prelude hiding ((.), id)
import qualified Prelude
import           Control.Category
import           Data.Kind

--------------------------------------------------

data List a = Nil | Cons a (List a)

data ListF a r = NilF | ConsF a r

cataList :: (ListF a b -> b) -> (List a -> b)
cataList f = go
  where
    go Nil = f NilF
    go (Cons x xs) = f (ConsF x (go xs))

deriving instance Functor (ListF a)

newtype Fix f = In
  { out :: f (Fix f)
  }

cata :: Functor f => (f b -> b) -> (Fix f -> b)
cata f = go
  where
    go = f . fmap go . out
