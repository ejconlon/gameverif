{-# LANGUAGE UndecidableInstances #-}

-- | Annotations on functors
module Gameverif.Util.Ann where

import Control.Monad.Reader (ReaderT (..), asks)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Gameverif.Util.Adjunction (Adjunction (..), WrappedF (..))
import Gameverif.Util.TwoLevel (Base1, Corecursive1 (..), Recursive1 (..), TwoLevel (..))

data Located p a = Located
  { locPos :: !p
  , locVal :: a
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Adjunction (Located p) where
  type AdjointInfo (Located p) = p
  unit = asks . flip Located
  rightAdjunctT h (Located p x) = runReaderT (h x) p

newtype AnnF p f v a = AnnF { unAnnF :: Located p (f v a) }
  deriving stock (Show)
  deriving newtype (Eq)

instance Functor (f v) => Functor (AnnF p f v) where
  fmap f (AnnF (Located p x)) = AnnF (Located p (fmap f x))

instance Bifunctor f => Bifunctor (AnnF p f) where
  bimap f g (AnnF (Located p x)) = AnnF (Located p (bimap f g x))

instance Bifoldable f => Bifoldable (AnnF p f) where
  bifoldr f g z (AnnF (Located _ x)) = bifoldr f g z x

instance Bitraversable f => Bitraversable (AnnF p f) where
  bitraverse f g (AnnF (Located p x)) = AnnF . Located p <$> bitraverse f g x

annPosF :: AnnF p f v a -> p
annPosF = locPos . unAnnF

annBodyF :: AnnF p f v a -> f v a
annBodyF = locVal . unAnnF

instance WrappedF (AnnF p f v) where
  type WrappedFuncF (AnnF p f v) = f v
  type WrappedInfoF (AnnF p f v) = p
  wrapUnitF = fmap AnnF . unit
  wrapInnerFT h = rightAdjunctT h . unAnnF

newtype Ann p f v = Ann { unAnn :: AnnF p f v (Ann p f v) }
  deriving (Functor, Foldable) via (TwoLevel (Ann p f))

deriving stock instance (Eq p, Eq (f v (Ann p f v))) => Eq (Ann p f v)
deriving stock instance (Show p, Show (f v (Ann p f v))) => Show (Ann p f v)

annPos :: Ann p f v -> p
annPos = annPosF . unAnn

annBody :: Ann p f v -> f v (Ann p f v)
annBody = annBodyF . unAnn

type instance Base (Ann p f v) = AnnF p f v

instance Functor (f v) => Recursive (Ann p f v) where
  project = unAnn

instance Functor (f v) => Corecursive (Ann p f v) where
  embed = Ann

type instance Base1 (Ann p f) = AnnF p f

instance Bifunctor f => Recursive1 (Ann p f) where
  project1 = unAnn

instance Bifunctor f => Corecursive1 (Ann p f) where
  embed1 = Ann

instance Bitraversable f => Traversable (Ann p f) where
  traverse f = fmap unTwoLevel . traverse f . TwoLevel
