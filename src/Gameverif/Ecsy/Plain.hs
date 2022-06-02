module Gameverif.Ecsy.Plain where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Sequence (Seq)
import Gameverif.Ecsy.Base (ExpF, ProgDecl, StmtSeqF)
import qualified Gameverif.Viper.Plain as VP

newtype Exp v = Exp { unExp :: ExpF v (Exp v) }
  deriving stock (Show)
  deriving newtype (Eq)

type instance Base (Exp v) = (ExpF v)

instance Recursive (Exp v) where
  project = unExp

instance Corecursive (Exp v) where
  embed = Exp

newtype StmtSeq v = StmtSeq { unStmtSeq :: StmtSeqF VP.Exp Exp v (StmtSeq v) }
  deriving stock (Show)
  deriving newtype (Eq)

type instance Base (StmtSeq v) = StmtSeqF VP.Exp Exp v

instance Recursive (StmtSeq v) where
  project = unStmtSeq

instance Corecursive (StmtSeq v) where
  embed = StmtSeq

type PlainDecl v = ProgDecl VP.Exp Exp StmtSeq v

type PlainProg v = Seq (PlainDecl v)
