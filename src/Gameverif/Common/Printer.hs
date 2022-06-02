module Gameverif.Common.Printer where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
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

clause :: (x -> Doc ann) -> Text -> x -> Doc ann
clause f txt x = P.hsep [P.pretty txt, P.parens (f x)]

clauses :: Foldable f => (x -> Doc ann) -> Text -> f x -> Maybe (Doc ann)
clauses f txt xs =
  let ys = toList xs
  in if null ys then Nothing else Just (P.vsep (fmap (clause f txt) (toList xs)))
