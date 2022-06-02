module Gameverif.Common.Printer where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prettyprinter (Doc)
import qualified Prettyprinter as P

tabWidth :: Int
tabWidth = 2

indented :: Doc ann -> Doc ann
indented = P.indent tabWidth

mhcat :: [Maybe (Doc ann)] -> Doc ann
mhcat = P.hcat . catMaybes

mhsep :: [Maybe (Doc ann)] -> Doc ann
mhsep = P.hsep . catMaybes

mvsep :: [Maybe (Doc ann)] -> Doc ann
mvsep = P.vsep . catMaybes

commaSep :: Foldable f => (x -> Doc ann) -> f x -> Doc ann
commaSep f xs = P.hsep (P.punctuate P.comma (fmap f (toList xs)))

commaLineSep :: Foldable f => (x -> Doc ann) -> f x -> Doc ann
commaLineSep f xs = P.vsep (P.punctuate P.comma (fmap f (toList xs)))

nonEmpty :: Seq x -> Doc ann -> Maybe (Doc ann)
nonEmpty s d = if Seq.null s then Nothing else Just d
