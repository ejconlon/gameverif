module Gameverif.Viper.Plain where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq)
import Gameverif.TwoLevel (Base1, Corecursive1 (..), Recursive1 (..), TwoLevel (..))
import Gameverif.Viper.Base (ExpF, ProgDecl, StmtSeqF)

newtype Exp v = Exp { unExp :: ExpF v (Exp v) }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (Functor, Foldable) via (TwoLevel Exp)

type instance Base (Exp v) = (ExpF v)

instance Recursive (Exp v) where
  project = unExp

instance Corecursive (Exp v) where
  embed = Exp

type instance Base1 Exp = ExpF

instance Recursive1 Exp where
  project1 = unExp

instance Corecursive1 Exp where
  embed1 = Exp

instance Traversable Exp where
  traverse f = fmap unTwoLevel . traverse f . TwoLevel

newtype StmtSeq v = StmtSeq { unStmtSeq :: StmtSeqF Exp v (StmtSeq v) }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (Functor, Foldable) via (TwoLevel StmtSeq)

type instance Base (StmtSeq v) = StmtSeqF Exp v

instance Recursive (StmtSeq v) where
  project = unStmtSeq

instance Corecursive (StmtSeq v) where
  embed = StmtSeq

type instance Base1 StmtSeq = StmtSeqF Exp

instance Recursive1 StmtSeq where
  project1 = unStmtSeq

instance Corecursive1 StmtSeq where
  embed1 = StmtSeq

instance Traversable StmtSeq where
  traverse f = fmap unTwoLevel . traverse f . TwoLevel

type PlainDecl v = ProgDecl Exp StmtSeq v

type PlainProgram v = Seq (PlainDecl v)
