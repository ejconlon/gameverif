module Gameverif.Viper.Concrete where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq)
import Gameverif.Util.Adjunction (WrappedF (..))
import Gameverif.Util.Ann (AnnF (..), Located (..))
import Gameverif.Util.TwoLevel (Base1, Corecursive1 (..), Recursive1 (..), TwoLevel (..))
import Gameverif.Viper.Base (ExpF, ProgDecl, StmtSeqF, stmtSeqMapExp)
import Gameverif.Viper.Plain (Exp (..), StmtSeq (StmtSeq))

newtype AnnExpF p v a = AnnExpF { unAnnExpF :: AnnF p ExpF v a }
  deriving stock (Show)
  deriving newtype (Eq, Functor)

instance Bifunctor (AnnExpF p) where
  bimap f g (AnnExpF y) = AnnExpF (bimap f g y)

instance WrappedF (AnnExpF p v) where
  type WrappedFuncF (AnnExpF p v) = ExpF v
  type WrappedInfoF (AnnExpF p v) = p
  wrapUnitF = fmap AnnExpF . wrapUnitF
  wrapInnerFT h = wrapInnerFT h . unAnnExpF

newtype AnnExp p v = AnnExp { unAnnExp :: AnnExpF p v (AnnExp p v) }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (Functor) via (TwoLevel (AnnExp p))

type instance Base (AnnExp p v) = AnnExpF p v

instance Recursive (AnnExp p v) where
  project = unAnnExp

instance Corecursive (AnnExp p v) where
  embed = AnnExp

type instance Base1 (AnnExp p) = AnnExpF p

instance Recursive1 (AnnExp p) where
  project1 = unAnnExp

instance Corecursive1 (AnnExp p) where
  embed1 = AnnExp

mkAnnExp :: p -> ExpF v (AnnExp p v) -> AnnExp p v
mkAnnExp p e = AnnExp (AnnExpF (AnnF (Located p e)))

forgetAnnExp :: AnnExp p v -> Exp v
forgetAnnExp (AnnExp (AnnExpF (AnnF (Located _ e)))) = Exp (second forgetAnnExp e)

recallAnnExp :: p -> Exp v -> AnnExp p v
recallAnnExp p = go where
  go (Exp ef) = AnnExp (AnnExpF (AnnF (Located p (fmap go ef))))

newtype AnnStmtSeqF p v a = AnnStmtSeqF { unAnnStmtSeqF :: AnnF p (StmtSeqF (AnnExp p)) v a }
  deriving stock (Show)
  deriving newtype (Eq, Functor)

instance Bifunctor (AnnStmtSeqF p) where
  bimap f g (AnnStmtSeqF y) = AnnStmtSeqF (bimap f g y)

instance WrappedF (AnnStmtSeqF p v) where
  type WrappedFuncF (AnnStmtSeqF p v) = StmtSeqF (AnnExp p) v
  type WrappedInfoF (AnnStmtSeqF p v) = p
  wrapUnitF = fmap AnnStmtSeqF . wrapUnitF
  wrapInnerFT h = wrapInnerFT h . unAnnStmtSeqF

newtype AnnStmtSeq p v = AnnStmtSeq { unAnnStmtSeq :: AnnStmtSeqF p v (AnnStmtSeq p v) }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (Functor) via (TwoLevel (AnnStmtSeq p))

type instance Base (AnnStmtSeq p v) = AnnStmtSeqF p v

instance Recursive (AnnStmtSeq p v) where
  project = unAnnStmtSeq

instance Corecursive (AnnStmtSeq p v) where
  embed = AnnStmtSeq

type instance Base1 (AnnStmtSeq p) = AnnStmtSeqF p

instance Recursive1 (AnnStmtSeq p) where
  project1 = unAnnStmtSeq

instance Corecursive1 (AnnStmtSeq p) where
  embed1 = AnnStmtSeq

mkAnnStmtSeq :: p -> StmtSeqF (AnnExp p) v (AnnStmtSeq p v) -> AnnStmtSeq p v
mkAnnStmtSeq p s = AnnStmtSeq (AnnStmtSeqF (AnnF (Located p s)))

forgetAnnStmtSeq :: AnnStmtSeq p v -> StmtSeq v
forgetAnnStmtSeq (AnnStmtSeq (AnnStmtSeqF (AnnF (Located _ s)))) = StmtSeq (stmtSeqMapExp forgetAnnExp (second forgetAnnStmtSeq s))

recallAnnStmtSeq :: p -> StmtSeq v -> AnnStmtSeq p v
recallAnnStmtSeq p = go where
  go (StmtSeq sf) = AnnStmtSeq (AnnStmtSeqF (AnnF (Located p (fmap go (stmtSeqMapExp (recallAnnExp p) sf)))))

type AnnProgDecl p v = Located p (ProgDecl (AnnExp p) (AnnStmtSeq p) v)

mkAnnProgDecl :: p -> ProgDecl (AnnExp p) (AnnStmtSeq p) v -> AnnProgDecl p v
mkAnnProgDecl = Located

type AnnProg p v = Seq (AnnProgDecl p v)
