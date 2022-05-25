{-# LANGUAGE UndecidableInstances #-}

module Gameverif.Util.TwoLevel where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)

type family Base1 (t :: Type -> Type) :: Type -> Type -> Type

class Bifunctor (Base1 t) => Recursive1 (t :: Type -> Type) where
  project1 :: t a -> Base1 t a (t a)

class Bifunctor (Base1 t) => Corecursive1 (t :: Type -> Type) where
  embed1 :: Base1 t a (t a) -> t a

newtype TwoLevel (t :: Type -> Type) (a :: Type) = TwoLevel { unTwoLevel :: t a }

type instance Base1 (TwoLevel t) = Base1 t

instance (Bifunctor (Base1 t), Recursive1 t) => Recursive1 (TwoLevel t) where
  project1 = second TwoLevel . project1 . unTwoLevel

instance (Bifunctor (Base1 t), Corecursive1 t) => Corecursive1 (TwoLevel t) where
  embed1 = TwoLevel . embed1 . second unTwoLevel

type instance Base (TwoLevel t a) = Base1 t a

instance (Functor (Base1 t a), Recursive1 t) => Recursive (TwoLevel t a) where
  project = project1

instance (Functor (Base1 t a), Corecursive1 t) => Corecursive (TwoLevel t a) where
  embed = embed1

instance (Bifunctor (Base1 t), Corecursive1 t, Recursive1 t) => Functor (TwoLevel t) where
  fmap f = go where
    go = embed1 . bimap f go . project1

instance (Bifoldable (Base1 t), Recursive1 t) => Foldable (TwoLevel t) where
  foldr f z0 t0 = go t0 z0 where
    go t z = bifoldr f go z (project1 t)

instance (Bitraversable (Base1 t), Corecursive1 t, Recursive1 t) => Traversable (TwoLevel t) where
  traverse f = go where
    go = fmap embed1 . bitraverse f go . project1
