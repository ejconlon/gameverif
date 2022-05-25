module Gameverif.Util.Adjunction where

import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (Reader, ReaderT (..), ask, asks, runReader)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)

readerFromTrans :: ReaderT r Identity a -> Reader r a
readerFromTrans m = asks (runIdentity . runReaderT m)

readerToTrans :: Monad m => Reader r a -> ReaderT r m a
readerToTrans m = asks (runReader m)

class Functor f => Adjunction (f :: Type -> Type) where
  type AdjointInfo f :: Type
  info :: f a -> AdjointInfo f
  info = rightAdjunct (const ask)
  unit :: a -> Reader (AdjointInfo f) (f a)
  rightAdjunct :: (a -> Reader (AdjointInfo f) b) -> (f a -> b)
  rightAdjunct h = runIdentity . rightAdjunctT (readerToTrans . h)
  unitT :: Monad m => a -> ReaderT (AdjointInfo f) m (f a)
  unitT = readerToTrans . unit
  rightAdjunctT :: (a -> ReaderT (AdjointInfo f) m b) -> (f a -> m b)

class WrappedF (f :: Type -> Type) where
  type WrappedFuncF f :: Type -> Type
  type WrappedInfoF f :: Type
  wrapUnitF :: WrappedFuncF f a -> Reader (WrappedInfoF f) (f a)
  wrapInnerF :: (WrappedFuncF f a -> Reader (WrappedInfoF f) b) -> (f a -> b)
  wrapInnerF h = runIdentity . wrapInnerFT (readerToTrans . h)
  wrapUnitFT :: Monad m => WrappedFuncF f a -> ReaderT (WrappedInfoF f) m (f a)
  wrapUnitFT = readerToTrans . wrapUnitF
  wrapInnerFT :: (WrappedFuncF f a -> ReaderT (WrappedInfoF f) m b) -> (f a -> m b)

type WrappedFunc t = WrappedFuncF (Base t)
type WrappedInfo t = WrappedInfoF (Base t)

wrapInfo :: (Recursive t, WrappedF (Base t)) => t -> WrappedInfo t
wrapInfo = wrapInner (const ask)

wrapUnit :: (Corecursive t, WrappedF (Base t)) => WrappedFunc t t -> Reader (WrappedInfo t) t
wrapUnit = fmap embed . wrapUnitF

wrapInner :: (Recursive t, WrappedF (Base t)) => (WrappedFunc t t -> Reader (WrappedInfo t) b) -> (t -> b)
wrapInner h = wrapInnerF h . project

wrapUnitT :: (Monad m, Corecursive t, WrappedF (Base t)) => WrappedFunc t t -> ReaderT (WrappedInfo t) m t
wrapUnitT = fmap embed . wrapUnitFT

wrapInnerT :: (Recursive t, WrappedF (Base t)) => (WrappedFunc t t -> ReaderT (WrappedInfo t) m b) -> (t -> m b)
wrapInnerT h = wrapInnerFT h . project

cataWrapped :: (Recursive t, f ~ Base t, WrappedF f) => (WrappedFuncF f (Reader (WrappedInfoF f) a) -> Reader (WrappedInfoF f) a) -> t -> a
cataWrapped g t = runReader (cata (wrapInnerF (pure . g)) t) (wrapInfo t)

cataWrappedM :: (Recursive t, f ~ Base t, WrappedF f) => (WrappedFuncF f (ReaderT (WrappedInfoF f) m a) -> ReaderT (WrappedInfoF f) m a) -> t -> m a
cataWrappedM g t = runReaderT (cata (wrapInnerF (pure . g)) t) (wrapInfo t)
