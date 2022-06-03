{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Ecsy.Parser where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Gameverif.Common.Lexer (Tok (..))
import Gameverif.Common.Parser (Extent, ParserM, betweenBracesP, betweenParensP, colonEqualsP, colonP, commaP, identP,
                                keywordP, keywordPred, lexP, optByKeywordP, periodP, withPos)
import Gameverif.Ecsy.Base (Access (..), Action (..), ArchDecl (..), ArchName (..), BoundArg (..), BoundRes (..),
                            CompDecl (..), CompField (..), CompName (..), FieldName (..), FuncDecl (..), FuncName (..),
                            InvDecl (..), InvName (..), LitTy (..), Local (..), MainDecl (..), MethDecl (..),
                            MethName (..), ProgDecl (..), QueryAttr (..), QueryDecl (..), QueryName (..), ResDecl (..),
                            ResName (..), StmtF (..), StmtSeqF (..), SysDecl (..), SysName (..), Ty (..), VarName (..))
import Gameverif.Ecsy.Concrete (AnnExp (..), AnnProg, AnnProgDecl, AnnStmtSeq (..), mkAnnProgDecl, mkAnnStmtSeq)
import qualified Gameverif.Viper.Parser as VP
import qualified Gameverif.Viper.Parser as VX
import SimpleParser (MatchBlock (..), MatchCase (..), anyToken, greedyPlusParser, greedyStarParser, lookAheadMatch,
                     peekToken, sepByParser)

type EcsyExp = AnnExp Extent
type EcsyStmtSeq = AnnStmtSeq Extent
type EcsyProgDecl a = AnnProgDecl Extent a
type EcsyProg a = AnnProg Extent a

funcNP :: ParserM FuncName
funcNP = fmap FuncName identP

queryNP :: ParserM QueryName
queryNP = fmap QueryName identP

sysNP :: ParserM SysName
sysNP = fmap SysName identP

compNP :: ParserM CompName
compNP = fmap CompName identP

invNP :: ParserM InvName
invNP = fmap InvName identP

archNP :: ParserM ArchName
archNP = fmap ArchName identP

fieldNP :: ParserM FieldName
fieldNP = fmap FieldName identP

varNP :: ParserM VarName
varNP = fmap VarName identP

resNP :: ParserM ResName
resNP = fmap ResName identP

methNP :: ParserM MethName
methNP = fmap MethName identP

boundArgP :: ParserM BoundArg
boundArgP = do
  name <- varNP
  colonP
  ty <- tyP
  let x = BoundArg name ty
  pure x

boundArgsP :: ParserM (Seq BoundArg)
boundArgsP = sepByParser boundArgP commaP

boundResP :: ParserM BoundRes
boundResP = do
  mac <- optByKeywordP "mut" (pure AccessMut)
  let ac = fromMaybe AccessConst mac
  name <- varNP
  colonP
  res <- resNP
  let x = BoundRes name ac res
  pure x

boundResesP :: ParserM (Seq BoundRes)
boundResesP = sepByParser boundResP commaP

funcP :: ParserM (FuncDecl VP.VipExp EcsyStmtSeq Text)
funcP = do
  keywordP "function"
  name <- funcNP
  args <- betweenParensP boundArgsP
  colonP
  retTy <- tyP
  mresources <- optClauseP "resources" boundResesP
  let resources = fromMaybe Seq.empty mresources
  requires <- propClausesP "requires"
  ensures <- propClausesP "ensures"
  mbody <- do
    z <- peekToken
    case z of
      Just TokOpenBrace -> Just <$> betweenBracesP stmtSeqP
      _ -> pure Nothing
  let x = FuncDecl name args retTy resources requires ensures mbody
  pure x

methDeclP :: ParserM (MethDecl VP.VipExp Text)
methDeclP = do
  keywordP "method"
  mac <- optByKeywordP "mut" (pure AccessMut)
  let ac = fromMaybe AccessConst mac
  name <- methNP
  args <- betweenParensP boundArgsP
  colonP
  retTy <- tyP
  requires <- propClausesP "requires"
  ensures <- propClausesP "ensures"
  let x = MethDecl name ac args retTy requires ensures
  pure x

resP :: ParserM (ResDecl VP.VipExp Text)
resP = do
  keywordP "resource"
  name <- resNP
  mparams <- optClauseP "parameters" boundArgsP
  let params = fromMaybe Seq.empty mparams
  requires <- propClausesP "requires"
  meths <- betweenBracesP (greedyStarParser methDeclP)
  let x = ResDecl name params requires meths
  pure x

fieldP :: ParserM CompField
fieldP = do
  name <- fieldNP
  colonP
  ty <- tyP
  let x = CompField name ty
  pure x

compP :: ParserM CompDecl
compP = do
  keywordP "component"
  name <- compNP
  fields <- betweenBracesP (greedyStarParser fieldP)
  let x = CompDecl name fields
  pure x

archP :: ParserM ArchDecl
archP = do
  keywordP "archetype"
  name <- archNP
  comps <- betweenBracesP (greedyStarParser compNP)
  let x = ArchDecl name comps
  pure x

queryAttrP :: ParserM QueryAttr
queryAttrP = yesMut <|> noMut where
  yesMut = keywordP "mut" *> fmap (`QueryAttr` AccessMut) compNP
  noMut = fmap (`QueryAttr` AccessConst) compNP

queryP :: ParserM QueryDecl
queryP = do
  keywordP "query"
  name <- queryNP
  attrs <- betweenBracesP (greedyStarParser queryAttrP)
  let x = QueryDecl name attrs
  pure x

invP :: ParserM (InvDecl VP.VipExp Text)
invP = do
  keywordP "invariant"
  name <- invNP
  body <- betweenBracesP VP.vipExpParser
  let x = InvDecl name body
  pure x

queriesP :: ParserM (Seq QueryName)
queriesP = sepByParser queryNP commaP

sysP :: ParserM (SysDecl VP.VipExp EcsyStmtSeq Text)
sysP = do
  keywordP "system"
  name <- sysNP
  mparams <- optClauseP "parameters" boundArgsP
  let params = fromMaybe Seq.empty mparams
  mresources <- optClauseP "resources" boundResesP
  let resources = fromMaybe Seq.empty mresources
  mqueries <- optClauseP "queries" queriesP
  let queries = fromMaybe Seq.empty mqueries
  requires <- propClausesP "requires"
  ensures <- propClausesP "ensures"
  body <- betweenBracesP stmtSeqP
  let x = SysDecl name params resources queries requires ensures body
  pure x

clauseP :: Text -> ParserM a -> ParserM a
clauseP kw p = do
  keywordP kw
  betweenParensP p

optClauseP :: Text -> ParserM a -> ParserM (Maybe a)
optClauseP kw p = optByKeywordP kw (betweenParensP p)

clausesP :: Text -> ParserM a -> ParserM (Seq a)
clausesP kw p = greedyStarParser (clauseP kw p)

propClauseP :: Text -> ParserM (VP.VipExp Text)
propClauseP kw = clauseP kw VP.vipExpParser

propClausesP :: Text -> ParserM (Seq (VP.VipExp Text))
propClausesP kw = clausesP kw VP.vipExpParser

mainP :: ParserM (MainDecl VP.VipExp EcsyStmtSeq Text)
mainP = do
  keywordP "main"
  mparams <- optClauseP "parameters" boundArgsP
  let params = fromMaybe Seq.empty mparams
  requires <- propClausesP "requires"
  ensures <- propClausesP "ensures"
  body <- betweenBracesP stmtSeqP
  let x = MainDecl params requires ensures body
  pure x

expP :: ParserM (EcsyExp Text)
expP = error "TODO"

accessP :: ParserM Access
accessP = AccessMut <$ keywordP "mut" <|> AccessConst <$ keywordP "const"

localP :: ParserM (Local EcsyExp Text)
localP = do
  access <- accessP
  name <- identP
  mty <- do
    z <- peekToken
    case z of
      Just TokColon -> do
        colonP
        Just <$> tyP
      _ -> pure Nothing
  colonEqualsP
  ex <- expP
  let x = Local (VarName name) access mty ex
  pure x

tyP :: ParserM Ty
tyP = TyLit LitTyUnit <$ keywordP "Unit"
  <|> TyLit LitTyInt <$ keywordP "Int"
  <|> TyLit LitTyBool <$ keywordP "Bool"
  <|> TyComp . CompName <$> identP

actionP :: ParserM (Action VP.VipExp EcsyExp Text)
actionP = asum [assertP, assignVarsP, assignFieldP] where
  assertP = do
    keywordP "assert"
    ActionAssert <$> VX.vipExpParser
  assignVarsP = do
    vs <- sepByParser identP commaP
    guard (not (Seq.null vs))
    colonEqualsP
    e <- expP
    let x = ActionAssignVars vs e
    pure x
  assignFieldP = do
    v <- identP
    periodP
    f <- identP
    colonEqualsP
    e <- expP
    let x = ActionAssignField v (FieldName f) e
    pure x

stmtP :: ParserM a -> ParserM (StmtF VP.VipExp EcsyExp Text a)
stmtP p = stmtBaseP where
  stmtBaseP = asum [stmtActionP, stmtLocalF, stmtIfP, stmtWhileP]
  stmtActionP = StmtActionF <$> actionP
  stmtLocalF = do
    vars <- greedyPlusParser localP
    body <- p
    let x = StmtLocalF vars body
    pure x
  stmtIfP = do
    keywordP "if"
    g <- betweenParensP expP
    t <- betweenBracesP p
    me <- optByKeywordP "else" (betweenBracesP p)
    let x = StmtIfF g t me
    pure x
  stmtWhileP = do
    keywordP "while"
    g <- betweenParensP expP
    is <- propClausesP "invariant"
    body <- betweenBracesP p
    let x = StmtWhileF g is body
    pure x

stmtSeqP :: ParserM (EcsyStmtSeq Text)
stmtSeqP = recP where
  recP = withPos mkAnnStmtSeq (consP <|> nilP)
  consP = StmtSeqConsF <$> stmtP recP <*> recP
  nilP = lexP (pure StmtSeqNilF)

ecsyDeclParser :: ParserM (EcsyProgDecl Text)
ecsyDeclParser = withPos mkAnnProgDecl (lookAheadMatch block) where
  block = MatchBlock anyToken (fail "invalid declaration")
    [ MatchCase Nothing (keywordPred "function") (fmap ProgDeclFunc funcP)
    , MatchCase Nothing (keywordPred "resource") (fmap ProgDeclRes resP)
    , MatchCase Nothing (keywordPred "component") (fmap ProgDeclComp compP)
    , MatchCase Nothing (keywordPred "archetype") (fmap ProgDeclArch archP)
    , MatchCase Nothing (keywordPred "query") (fmap ProgDeclQuery queryP)
    , MatchCase Nothing (keywordPred "invariant") (fmap ProgDeclInv invP)
    , MatchCase Nothing (keywordPred "system") (fmap ProgDeclSys sysP)
    , MatchCase Nothing (keywordPred "main") (fmap ProgDeclMain mainP)
    ]

ecsyProgParser :: ParserM (EcsyProg Text)
ecsyProgParser = greedyStarParser ecsyDeclParser
