{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Ecsy.Parser where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Gameverif.Common.Lexer (Atom (..), Tok (..))
import Gameverif.Common.Parser (Extent, ParserM, betweenBracesP, betweenParensP, colonEqualsP, colonP, commaP, identP,
                                keywordP, keywordPred, lexP, periodP, withPos)
import Gameverif.Ecsy.Base (Access (..), Action (..), ArchDecl (..), ArchName (..), CompDecl, CompName (..),
                            FieldName (..), FuncDecl, InvDecl (..), InvName (..), LitTy (..), Local (..), MainDecl (..),
                            ProgDecl (..), QueryAttr (..), QueryDecl (..), QueryName (..), ResDecl, StmtF (..),
                            StmtSeqF (..), SysDecl, Ty (..), VarName (..))
import Gameverif.Ecsy.Concrete (AnnExp (..), AnnProg, AnnProgDecl, AnnStmtSeq (..), mkAnnProgDecl, mkAnnStmtSeq)
import qualified Gameverif.Viper.Parser as VP
import qualified Gameverif.Viper.Parser as VX
import SimpleParser (MatchBlock (..), MatchCase (..), anyToken, greedyPlusParser, greedyStarParser, lookAheadMatch,
                     peekToken, sepByParser)

type EcsyExp = AnnExp Extent
type EcsyStmtSeq = AnnStmtSeq Extent
type EcsyProgDecl a = AnnProgDecl Extent a
type EcsyProg a = AnnProg Extent a

queryNP :: ParserM QueryName
queryNP = fmap QueryName identP

compNP :: ParserM CompName
compNP = fmap CompName identP

invNP :: ParserM InvName
invNP = fmap InvName identP

archNP :: ParserM ArchName
archNP = fmap ArchName identP

funcP :: ParserM (FuncDecl VP.VipExp EcsyStmtSeq Text)
funcP = error "TODO"

resP :: ParserM (ResDecl VP.VipExp Text)
resP = error "TODO"

compP :: ParserM CompDecl
compP = error "TODO"

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

sysP :: ParserM (SysDecl VP.VipExp EcsyStmtSeq Text)
sysP = error "TODO"

propClauseP :: Text -> ParserM (VP.VipExp Text)
propClauseP kw = do
  keywordP kw
  betweenParensP VP.vipExpParser

propClausesP :: Text -> ParserM (Seq (VP.VipExp Text))
propClausesP = greedyStarParser . propClauseP

mainP :: ParserM (MainDecl VP.VipExp EcsyStmtSeq Text)
mainP = do
  keywordP "main"
  ensures <- propClausesP "ensures"
  body <- betweenBracesP stmtSeqP
  let x = MainDecl ensures body
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
    me <- do
      z <- peekToken
      case z of
        Just (TokAtom (AtomIdent "else")) -> do
          keywordP "else"
          Just <$> betweenBracesP p
        _ -> pure Nothing
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
