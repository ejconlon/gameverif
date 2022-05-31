module Gameverif.Viper.Printer where

import Data.Foldable (toList)
import Gameverif.Viper.Base (DomDecl (..), FieldDecl (..), FuncDecl (..), MethDecl (..), PredDecl (..), ProgDecl (..))
import Gameverif.Viper.Plain (Exp, PlainDecl, PlainProg, StmtSeq)
import Prettyprinter

-- TODO

printProg :: Pretty v => PlainProg v -> Doc ann
printProg = vsep . fmap printDecl . toList

printDecl :: Pretty v => PlainDecl v -> Doc ann
printDecl = \case
  ProgDeclField fd -> printFieldDecl fd
  ProgDeclDom dd -> printDomDecl dd
  ProgDeclFunc fd -> printFuncDecl fd
  ProgDeclMeth md -> printMethDecl md
  ProgDeclPred pd -> printPredDecl pd

printFieldDecl :: FieldDecl -> Doc ann
printFieldDecl (FieldDecl fn ty) = emptyDoc

printDomDecl :: Pretty v => DomDecl Exp v -> Doc ann
printDomDecl (DomDecl tn seq seq') = emptyDoc

printFuncDecl :: FuncDecl Exp v -> Doc ann
printFuncDecl (FuncDecl fn seq ty seq' seq2 exp) = emptyDoc

printMethDecl :: MethDecl Exp StmtSeq v -> Doc ann
printMethDecl (MethDecl mn seq seq' seq2 seq3 ss) = emptyDoc

printPredDecl :: PredDecl Exp v -> Doc ann
printPredDecl (PredDecl pn seq exp) = emptyDoc
