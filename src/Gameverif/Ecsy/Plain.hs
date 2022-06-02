module Gameverif.Ecsy.Plain where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq)
import Gameverif.Ecsy.Base (ExpF, ProgDecl, StmtSeqF)
import Gameverif.Util.TwoLevel (Base1, Corecursive1 (..), Recursive1 (..), TwoLevel (..))
import qualified Gameverif.Viper.Plain as VP

newtype Exp v = Exp { unExp :: ExpF v (Exp v) }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (Functor) via (TwoLevel Exp)

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

newtype StmtSeq v = StmtSeq { unStmtSeq :: StmtSeqF VP.Exp Exp v (StmtSeq v) }
  deriving stock (Show)
  deriving newtype (Eq)
  deriving (Functor) via (TwoLevel StmtSeq)

type instance Base (StmtSeq v) = StmtSeqF VP.Exp Exp v

instance Recursive (StmtSeq v) where
  project = unStmtSeq

instance Corecursive (StmtSeq v) where
  embed = StmtSeq

type instance Base1 StmtSeq = StmtSeqF VP.Exp Exp

instance Recursive1 StmtSeq where
  project1 = unStmtSeq

instance Corecursive1 StmtSeq where
  embed1 = StmtSeq

type PlainDecl v = ProgDecl VP.Exp StmtSeq v

type PlainProg v = Seq (PlainDecl v)
