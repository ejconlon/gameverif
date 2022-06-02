{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Ecsy.Printer where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Gameverif.Common.Printer (commaLineSep, commaSep, indented, mvsep, nonEmpty)
import Gameverif.Ecsy.Base (Access (..), ArchDecl (..), ArchName (..), BoundArg (..), BoundRes (..), BuiltOp (..),
                            CompDecl (..), CompField (..), CompName (..), ExpF (..), FieldName (..), FuncDecl (..),
                            InvDecl (..), InvName (..), Lit (..), LitTy (..), Local (..), MainDecl (..), MethDecl (..),
                            Op (..), ProgDecl (..), QueryAttr (..), QueryDecl (..), QueryName (..), ResDecl (..),
                            ResName (..), StmtF (..), StmtSeqF (..), SysDecl (..), SysName (..), Ty (..), VarName (..),
                            opArity)
import Gameverif.Ecsy.Plain (Exp (..), PlainDecl, PlainProg, StmtSeq (..))
import qualified Gameverif.Viper.Plain as VP
import qualified Gameverif.Viper.Printer as VX
import Prettyprinter (Doc, Pretty)
import qualified Prettyprinter as P

printProg :: Pretty v => PlainProg v -> Doc ann
printProg = P.vsep . P.punctuate P.line . fmap printDecl . toList

printDecl :: Pretty v => PlainDecl v -> Doc ann
printDecl = \case
  ProgDeclFunc fd -> printFuncDecl fd
  ProgDeclRes rd -> printResDecl rd
  ProgDeclSys sd -> printSysDecl sd
  ProgDeclQuery qd -> printQueryDecl qd
  ProgDeclComp cd -> printCompDecl cd
  ProgDeclArch ad -> printArchDecl ad
  ProgDeclInv vd -> printInvDecl vd
  ProgDeclMain md -> printMainDecl md

printMainDecl :: Pretty v => MainDecl VP.Exp StmtSeq v -> Doc ann
printMainDecl (MainDecl ensures body) = mvsep
  [ Just "main"
  , nonEmpty ensures (P.vsep (fmap VX.printEn (toList ensures)))
  , Just P.lbrace
  , Just (indented (printStmtSeq body))
  , Just P.rbrace
  ]

printLocal :: Pretty v => Local Exp v -> Doc acc
printLocal (Local vn ty rhs) = P.hcat [P.pretty (unVarName vn), P.colon, P.space, printTy ty, P.space, printExp rhs]

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
  ExpLetF v a b -> P.hsep ["let", P.pretty v, "==", P.parens (printExp a), "in", P.parens (printExp b)]

printBuiltOp :: BuiltOp -> Doc ann
printBuiltOp = \case
  BuiltOpEquals -> "=="
  BuiltOpNotEquals -> "!="
  BuiltOpNot -> "!"
  BuiltOpAnd -> "&&"
  BuiltOpOr -> "||"
  BuiltOpAdd -> "+"
  BuiltOpSub -> "-"
  BuiltOpMul -> "*"
  BuiltOpDiv -> "/"
  BuiltOpMod -> "%"
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

printLit :: Lit -> Doc ann
printLit = \case
  LitUnit -> "unit"
  LitInt n -> P.pretty n
  LitBool b -> if b then "true" else "false"

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
      , nonEmpty invs (P.vsep (fmap VX.printInv (toList invs)))
      , Just P.lbrace
      , Just (indented (printStmtSeq body))
      , Just P.rbrace
      ]
    StmtAssertF prop -> P.hsep ["assert", VX.printExp prop]

printInvDecl :: Pretty v => InvDecl VP.Exp v -> Doc ann
printInvDecl (InvDecl name prop) = P.vsep
  [ P.hsep ["invariant", P.pretty (unInvName name)]
  , P.lbrace
  , indented (VX.printExp prop)
  , P.rbrace
  ]

printArchDecl :: ArchDecl -> Doc ann
printArchDecl (ArchDecl name comps) = P.vsep
  [ P.hsep ["archetype", P.pretty (unArchName name)]
  , P.lbrace
  , indented (commaLineSep (P.pretty . unCompName) comps)
  , P.rbrace
  ]

printCompDecl :: CompDecl -> Doc ann
printCompDecl (CompDecl name fields) = P.vsep
  [ P.hsep ["component", P.pretty (unCompName name)]
  , P.lbrace
  , indented (commaLineSep printCompField fields)
  , P.rbrace
  ]

printCompField :: CompField -> Doc ann
printCompField (CompField fn ty) = P.hcat [P.pretty (unFieldName fn), P.colon, P.space, printTy ty]

printQueryDecl :: QueryDecl -> Doc ann
printQueryDecl (QueryDecl name attrs) = P.vsep
  [ P.hsep ["query", P.pretty (unQueryName name)]
  , P.lbrace
  , indented (commaLineSep printQueryAttr attrs)
  , P.rbrace
  ]

printQueryAttr :: QueryAttr -> Doc ann
printQueryAttr (QueryAttr name access) =
  let ndoc = P.pretty (unCompName name)
  in case access of
    AccessConst -> ndoc
    AccessMut -> P.hsep ["mut", ndoc]

printSysDecl :: SysDecl StmtSeq v -> Doc ann
printSysDecl (SysDecl name params resources queries body) = mvsep
  [ Just (P.hsep ["system", P.pretty (unSysName name)])
  , nonEmpty params (P.hsep ["parameters", P.parens (commaSep printBoundArg params)])
  , nonEmpty resources (P.hsep ["resources", P.parens (commaSep printBoundRes resources)])
  , nonEmpty queries (P.hsep ["queries", P.parens (commaSep (P.pretty . unQueryName) queries)])
  ]

printBoundRes :: BoundRes -> Doc ann
printBoundRes (BoundRes vn rn ac) =
  let ndoc = P.pretty (unVarName vn)
      rdoc = P.pretty (unResName rn)
      tl = P.hcat [ndoc, P.colon, P.space, rdoc]
  in case ac of
    AccessConst -> tl
    AccessMut -> P.hsep ["mut", tl]

printBoundArg :: BoundArg -> Doc ann
printBoundArg (BoundArg vn ty) = P.hcat [P.pretty (unVarName vn), P.colon, P.space, printTy ty]

printResDecl :: ResDecl VP.Exp v -> Doc ann
printResDecl (ResDecl name ctorArgs methods) = mempty -- TODO

printFuncDecl :: FuncDecl VP.Exp StmtSeq v -> Doc ann
printFuncDecl (FuncDecl name args resources retTy requires ensures body) = mempty -- TODO

printMethDecl :: MethDecl VP.Exp v -> Doc ann
printMethDecl (MethDecl name args resources retTy requires ensures) = mempty -- TODO

printTy :: Ty -> Doc ann
printTy = \case
  TyLit lt -> printLitTy lt
  TyComp tn -> P.pretty (unCompName tn)

printLitTy :: LitTy -> Doc ann
printLitTy = \case
  LitTyUnit -> "Unit"
  LitTyInt -> "Int"
  LitTyBool -> "Bool"

{-

function foo(value: int): int
  resources (mut foo: Screen)
  requires ($prop)
  ensures ($prop)
{
  $stmts
}

resource Screen {
  init(value: int)

  method mut drawCircle(x: int, y: int, r: int): unit
    requires ($prop)
    ensures ($prop)
}

component Position
{
  x: int,
  y: int
}

archetype Player
{
  Player,
  Position
}

invariant name { $exp }

query Blah
{
  mut Player,
  Position
}

system movement
  parameters (value: int)
  resources (mut foo: Screen)
  queries (Blah, Bar, Baz)
  requires ($prop)
  ensures ($prop)
{
  $stmt
}

main
ensures ($prop)
{
  $stmts
}

-}
