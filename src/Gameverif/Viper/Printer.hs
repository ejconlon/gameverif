{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Viper.Printer where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Gameverif.Viper.Base (ArgDecl (..), BuiltOp (..), DomDecl (..), ExpF (..), FieldDecl (..), FieldName (..),
                             FuncDecl (..), FuncName (..), Lit (..), LitTy (..), MethDecl (..), MethName (..), Op (..),
                             PredDecl (..), PredName (..), ProgDecl (..), Quant (..), QuantVar (..), Trigger, Ty (..),
                             TyName (..), VarName (..), opArity)
import Gameverif.Viper.Plain (Exp (..), PlainDecl, PlainProg, StmtSeq (..))
import Prettyprinter (Doc, Pretty)
import qualified Prettyprinter as P

tabWidth :: Int
tabWidth = 2

mhcat :: [Maybe (Doc ann)] -> Doc ann
mhcat = P.hcat . catMaybes

mhsep :: [Maybe (Doc ann)] -> Doc ann
mhsep = P.hsep . catMaybes

mvsep :: [Maybe (Doc ann)] -> Doc ann
mvsep = P.vsep . catMaybes

printProg :: Pretty v => PlainProg v -> Doc ann
printProg = P.vsep . P.punctuate P.line . fmap printDecl . toList

printDecl :: Pretty v => PlainDecl v -> Doc ann
printDecl = \case
  ProgDeclField fd -> printFieldDecl fd
  ProgDeclDom dd -> printDomDecl dd
  ProgDeclFunc fd -> printFuncDecl fd
  ProgDeclMeth md -> printMethDecl md
  ProgDeclPred pd -> printPredDecl pd

printTy :: Ty -> Doc ann
printTy = \case
  TyRef -> "Ref"
  TyLabel -> error "internal ty: label"
  TyLit lt -> printLitTy lt
  TyDomain tn -> P.pretty (unTyName tn)
  TyFun _ _ -> error "internal ty: fun"
  TyMeth _ _ -> error "internal ty: meth"
  TyTup ts -> P.hsep (fmap printTy (toList ts))

printLitTy :: LitTy -> Doc ann
printLitTy = \case
  LitTyInt -> "Int"
  LitTyBool -> "Bool"

printFieldDecl :: FieldDecl -> Doc ann
printFieldDecl (FieldDecl fn ty) = P.hcat ["field", P.space, P.pretty (unFieldName fn), P.colon, P.space, printTy ty]

printDomDecl :: Pretty v => DomDecl Exp v -> Doc ann
printDomDecl (DomDecl tn fns axs) = mvsep
  [ Just $ P.hsep ["domain", P.pretty (unTyName tn)]
  , Just P.lbrace
  -- TODO fns
  -- TODO axs
  , Just P.rbrace
  ]

printArg :: ArgDecl -> Doc ann
printArg (ArgDecl vn ty) = P.hcat [P.pretty (unVarName vn), P.colon, P.space, printTy ty]

printComma :: Foldable f => (x -> Doc ann) -> f x -> Doc ann
printComma f xs = P.hsep (P.punctuate P.comma (fmap f (toList xs)))

printBuiltOp :: BuiltOp -> Doc ann
printBuiltOp = \case
  BuiltOpEquals -> "=="
  BuiltOpNotEquals -> "!="
  BuiltOpImplies -> "==>"
  BuiltOpIff -> "<==>"
  BuiltOpNot -> "!"
  BuiltOpAnd -> "&&"
  BuiltOpOr -> "||"
  BuiltOpAdd -> "+"
  BuiltOpSub -> "-"
  BuiltOpMul -> "*"
  BuiltOpDiv -> "/"
  BuiltOpMod -> "%"
  BuiltOpWand -> "--*"
  BuiltOpGt -> ">"
  BuiltOpGte -> ">="
  BuiltOpLt -> "<"
  BuiltOpLte -> "<="

printBuiltApp :: Pretty v => BuiltOp -> Seq (Exp v) -> Doc ann
printBuiltApp bo args =
  let f = printBuiltOp bo
  in case opArity bo of
    1 ->
      let x = printExp (Seq.index args 0)
      in P.sep [f, P.parens x]
    2 ->
      let x = printExp (Seq.index args 0)
          y = printExp (Seq.index args 1)
      in P.hsep [P.parens x, f, P.parens y]
    _ -> error "invalid arity"

printQuantVar :: QuantVar -> Doc ann
printQuantVar (QuantVar vn ty) = P.hcat [P.pretty (unVarName vn), P.colon, P.space, printTy ty]

printTrigger :: Pretty v => Trigger (Exp v) -> Doc ann
printTrigger es = P.braces (printComma printExp es)

printExp :: Pretty v => Exp v -> Doc ann
printExp (Exp ef) = case ef of
  ExpVarF v -> P.pretty v
  ExpLitF l -> printLit l
  ExpCondF a b c -> P.hsep [printExp a, "?", printExp b, P.colon, printExp c]
  ExpAppF o args ->
    case o of
      OpFree v -> P.hcat [P.pretty v, P.parens (printComma printExp args)]
      OpBuilt bo -> printBuiltApp bo args
  ExpFieldF a n -> P.hcat [P.parens (printExp a), ".", P.pretty (unFieldName n)]
  ExpQuantF qu vs alts c ->
    let qn = case qu of { QuantExists -> "exists"; QuantForall -> "forall" }
        adoc = if Seq.null alts then Nothing else Just (P.hsep (fmap printTrigger (toList alts)))
    in mhsep [Just qn, Just (P.hsep (P.punctuate P.comma (fmap printQuantVar (toList vs)))), Just "::", adoc, Just (P.parens (printExp c))]
  ExpOldF mv a -> P.hcat ["old", maybe mempty (P.brackets . P.pretty) mv, P.parens (printExp a)]
  ExpUnfoldingF a b -> P.hsep ["unfolding", P.parens (printExp a), "in", P.parens (printExp b)]
  ExpLetF v a b -> P.hsep ["let", P.pretty v, "==", P.parens (printExp a), "in", P.parens (printExp b)]

printLit :: Lit -> Doc ann
printLit = \case
  LitInt n -> P.pretty n
  LitBool b -> if b then "true" else "false"

printStmtSeq :: Pretty v => StmtSeq v -> Doc ann
printStmtSeq = const "TODO"
-- printStmtSeq (StmtSeq ssf) = go ssf where
--   go = \case
--     StmtSeqNilF -> mempty
--     StmtSeqConsF sf ss -> "TODO"

printRq :: Pretty v => Exp v -> Doc ann
printRq e = P.indent tabWidth (P.hsep ["requires", P.parens (printExp e)])

printEn :: Pretty v => Exp v -> Doc ann
printEn e = P.indent tabWidth (P.hsep ["ensures", P.parens (printExp e)])

printFuncDecl :: Pretty v => FuncDecl Exp v -> Doc ann
printFuncDecl (FuncDecl fn args ret rqs ens body) = mvsep
  [ Just $ P.hcat ["function", P.space, P.pretty (unFuncName fn), P.parens (printComma printArg args), P.colon, P.space, printTy ret]
  , if Seq.null rqs then Nothing else Just (P.vsep (fmap printRq (toList rqs)))
  , if Seq.null ens then Nothing else Just (P.vsep (fmap printEn (toList ens)))
  , Just P.lbrace
  , Just (P.indent tabWidth (printExp body))
  , Just P.rbrace
  ]

printMethDecl :: Pretty v => MethDecl Exp StmtSeq v -> Doc ann
printMethDecl (MethDecl mn args rets rqs ens body) = mvsep
  [ Just $ P.hcat
      [ "method", P.space, P.pretty (unMethName mn), P.parens (printComma printArg args)
      , if Seq.null rets then mempty else P.hcat [P.space, "returns", P.space, P.parens (printComma printArg rets)]
      ]
  , if Seq.null rqs then Nothing else Just (P.vsep (fmap printRq (toList rqs)))
  , if Seq.null ens then Nothing else Just (P.vsep (fmap printEn (toList ens)))
  , Just P.lbrace
  , Just (P.indent tabWidth (printStmtSeq body))
  , Just P.rbrace
  ]

printPredDecl :: Pretty v => PredDecl Exp v -> Doc ann
printPredDecl (PredDecl pn args body) = mvsep
  [ Just $ P.hsep ["predicate", P.pretty (unPredName pn), P.parens (printComma printArg args)]
  , Just P.lbrace
  , Just (P.indent tabWidth (printExp body))
  , Just P.rbrace
  ]
