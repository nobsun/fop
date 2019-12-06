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
module HAlgebraAst where

import           Prelude hiding ((.), id)
import qualified Prelude
import           Control.Category
import           Data.Kind


newtype f :~> g = Nat { unNat :: forall i. f i -> g i }
newtype HFix f i = HIn { hout :: f (HFix f) i }

hcata :: HFunctor f => (f b :~> b) -> (HFix f :~> b)
hcata f = go
  where
    go = f . hfmap go . Nat hout

instance Category (:~>) where
  id = Nat Prelude.id
  Nat f . Nat g = Nat (f Prelude.. g)

class HFunctor (f :: (k -> Type) -> (k -> Type)) where
  hfmap :: (a :~> b) -> (f a :~> f b)

hfmapLack :: HFunctor f => a :~> b -> f a i -> f b i
hfmapLack f = unNat (hfmap f)


type VarName = String

data AstTag
  = ExprTag
  | DeclTag

data AstF r i where
  AstExprF :: ExprF r -> AstF r 'ExprTag
  AstDeclF :: DeclF r -> AstF r 'DeclTag

data ExprF r
  = LamAbsF VarName (r 'ExprTag)
  | AppF (r 'ExprTag) (r 'ExprTag)
  | LetF [r 'DeclTag] (r 'ExprTag)
  | VarF VarName
  | NumLitF Int

data DeclF r
  = DeclF VarName (r 'ExprTag)


instance HFunctor AstF where
  hfmap (Nat f) = Nat \case
    AstExprF (LamAbsF x e) -> AstExprF $ LamAbsF x (f e)
    AstExprF (AppF e1 e2)  -> AstExprF $ AppF (f e1) (f e2)
    AstExprF (LetF ds e)   -> AstExprF $ LetF (f <$> ds) (f e)
    AstExprF (VarF x)      -> AstExprF $ VarF x
    AstExprF (NumLitF x)   -> AstExprF $ NumLitF x
    AstDeclF (DeclF x e)   -> AstDeclF $ DeclF x (f e)

type Ast = HFix AstF

hcataAst :: (AstF b :~> b) -> (Ast :~> b)
hcataAst = hcata
