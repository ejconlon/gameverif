{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Viper.Printer where

import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Gameverif.Common.Printer (clauses, commaSep, indented, mhcat, mhsep, mvsep, nonEmpty)
import Gameverif.Viper.Base (Action (..), ArgDecl (..), AxName (..), AxiomDecl (..), BuiltOp (..), DomDecl (..),
                             DomFuncDecl (..), ExpF (..), FieldDecl (..), FieldName (..), FuncDecl (..), FuncName (..),
                             Lit (..), LitTy (..), Local (..), MethDecl (..), MethName (..), Op (..), PredDecl (..),
                             PredName (..), ProgDecl (..), ProofAction (..), Quant (..), QuantVar (..), StmtF (..),
                             StmtSeqF (..), Trigger, Ty (..), TyName (..), VarName (..), opArity)
import Gameverif.Viper.Plain (Exp (..), PlainDecl, PlainProg, StmtSeq (..))
import Prettyprinter (Doc, Pretty)
import qualified Prettyprinter as P

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
  , nonEmpty fns (indented (P.vsep (fmap printDomFunc (toList fns))))
  , nonEmpty axs (indented (P.vsep (fmap printAxiom (toList axs))))
  , Just P.rbrace
  ]

printAxiom :: Pretty v => AxiomDecl Exp v -> Doc ann
printAxiom (AxiomDecl man ex) = P.vsep
  [ mhsep [Just "axiom", fmap (P.pretty . unAxName) man]
  , P.lbrace
  , indented (printExp ex)
  , P.rbrace
  ]

printDomFunc :: DomFuncDecl -> Doc ann
printDomFunc (DomFuncDecl fn args ret) =
  P.hcat ["function", P.space, P.pretty (unFuncName fn), P.parens (commaSep printArg args), P.colon, P.space, printTy ret]

printArg :: ArgDecl -> Doc ann
printArg (ArgDecl vn ty) = P.hcat [P.pretty (unVarName vn), P.colon, P.space, printTy ty]

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
printTrigger es = P.braces (commaSep printExp es)

printExp :: Pretty v => Exp v -> Doc ann
printExp (Exp ef) = case ef of
  ExpVarF v -> P.pretty v
  ExpLitF l -> printLit l
  ExpCondF a b c -> P.hsep [printExp a, "?", printExp b, P.colon, printExp c]
  ExpAppF o args ->
    case o of
      OpFree v -> P.hcat [P.pretty v, P.parens (commaSep printExp args)]
      OpBuilt bo -> printBuiltApp bo args
  ExpFieldF a n -> P.hcat [P.parens (printExp a), ".", P.pretty (unFieldName n)]
  ExpQuantF qu vs alts c ->
    let qn = case qu of { QuantExists -> "exists"; QuantForall -> "forall" }
        adoc = nonEmpty alts (P.hsep (fmap printTrigger (toList alts)))
    in mhsep [Just qn, Just (P.hsep (P.punctuate P.comma (fmap printQuantVar (toList vs)))), Just "::", adoc, Just (P.parens (printExp c))]
  ExpOldF mv a -> P.hcat ["old", maybe mempty (P.brackets . P.pretty) mv, P.parens (printExp a)]
  ExpUnfoldingF a b -> P.hsep ["unfolding", P.parens (printExp a), "in", P.parens (printExp b)]
  ExpLetF v a b -> P.hsep ["let", P.pretty v, "==", P.parens (printExp a), "in", P.parens (printExp b)]

printLit :: Lit -> Doc ann
printLit = \case
  LitInt n -> P.pretty n
  LitBool b -> if b then "true" else "false"

printLocal :: Pretty v => Local Exp v -> Doc acc
printLocal (Local vn ty mrhs) = mhcat
  [ Just "var"
  , Just P.space
  , Just (P.pretty (unVarName vn))
  , Just P.colon
  , Just P.space
  , Just (printTy ty)
  , fmap (\rhs -> P.space <> P.colon <> P.equals <> P.space <> printExp rhs) mrhs
  ]

printAction :: Pretty v => Action Exp v -> Doc acc
printAction = \case
  ActionLabel v -> P.hsep ["label", P.pretty v]
  ActionProof pa ex ->
    let paDoc = case pa of
                  ProofActionAssert -> "assert"
                  ProofActionAssume -> "assume"
                  ProofActionInhale -> "inhale"
                  ProofActionExhale -> "exhale"
    in P.hsep [paDoc, printExp ex]
  ActionAssignVars vs ex -> P.hsep [commaSep P.pretty vs, P.colon <> P.equals, printExp ex]
  ActionAssignField v fn ex -> P.hcat [P.pretty v, P.dot, P.pretty (unFieldName fn), P.space, P.equals, P.space, printExp ex]
  ActionUnfold ex -> P.hsep ["unfold", printExp ex]
  ActionFold ex -> P.hsep ["fold", printExp ex]

printStmtSeq :: Pretty v => StmtSeq v -> Doc ann
printStmtSeq (StmtSeq ssf0) = go1 mempty ssf0 where
  go1 !doc = \case
    StmtSeqNilF -> doc
    StmtSeqConsF sf (StmtSeq ssf) ->
      let doc' = doc <> P.line <> go2 sf
      in go1 doc' ssf
  go2 = \case
    StmtLocalF locs (StmtSeq ssf) -> go1 (mconcat (P.punctuate P.line (fmap printLocal (toList locs)))) ssf
    StmtIfF cond thn mels ->
      let ifPart =
            P.vsep
              [ P.hsep ["if", P.parens (printExp cond)]
              , P.lbrace
              , indented (printStmtSeq thn)
              , P.rbrace
              ]
      in case mels of
          Nothing -> ifPart
          Just els ->
            let elsPart =
                  P.vsep
                    [ "else"
                    , P.lbrace
                    , indented (printStmtSeq els)
                    , P.rbrace
                    ]
            in P.vsep [ifPart, elsPart]
    StmtWhileF cond invs body -> mvsep
      [ Just (P.hsep ["while", P.parens (printExp cond)])
      , fmap indented (printInvs invs)
      , Just P.lbrace
      , Just (indented (printStmtSeq body))
      , Just P.rbrace
      ]
    StmtActionF ac -> printAction ac

printRqs :: Pretty v => Seq (Exp v) -> Maybe (Doc ann)
printRqs = clauses printExp "requires"

printEns :: Pretty v => Seq (Exp v) -> Maybe (Doc ann)
printEns = clauses printExp "ensures"

printInvs :: Pretty v => Seq (Exp v) -> Maybe (Doc ann)
printInvs = clauses printExp "invariant"

printFuncDecl :: Pretty v => FuncDecl Exp v -> Doc ann
printFuncDecl (FuncDecl fn args ret rqs ens body) = mvsep
  [ Just $ P.hcat ["function", P.space, P.pretty (unFuncName fn), P.parens (commaSep printArg args), P.colon, P.space, printTy ret]
  , fmap indented (printRqs rqs)
  , fmap indented (printEns ens)
  , Just P.lbrace
  , Just (indented (printExp body))
  , Just P.rbrace
  ]

printMethDecl :: Pretty v => MethDecl Exp StmtSeq v -> Doc ann
printMethDecl (MethDecl mn args rets rqs ens body) = mvsep
  [ Just $ P.hcat
      [ "method", P.space, P.pretty (unMethName mn), P.parens (commaSep printArg args)
      , if Seq.null rets then mempty else P.hcat [P.space, "returns", P.space, P.parens (commaSep printArg rets)]
      ]
  , fmap indented (printRqs rqs)
  , fmap indented (printEns ens)
  , Just P.lbrace
  , Just (indented (printStmtSeq body))
  , Just P.rbrace
  ]

printPredDecl :: Pretty v => PredDecl Exp v -> Doc ann
printPredDecl (PredDecl pn args body) = mvsep
  [ Just $ P.hcat ["predicate", P.space, P.pretty (unPredName pn), P.parens (commaSep printArg args)]
  , Just P.lbrace
  , Just (indented (printExp body))
  , Just P.rbrace
  ]
