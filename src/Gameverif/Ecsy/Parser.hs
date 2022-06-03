{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Ecsy.Parser where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Gameverif.Common.Parser (Extent, ParserM, closeBraceP, commaP, identP, keywordP, keywordPred, lexP, openBraceP,
                                withPos)
import Gameverif.Ecsy.Base (Access (..), ArchDecl, CompDecl, CompName (CompName), FuncDecl, InvDecl, MainDecl,
                            ProgDecl (..), QueryAttr (..), QueryDecl (..), QueryName (..), ResDecl, SysDecl)
import Gameverif.Ecsy.Concrete (AnnExp (..), AnnProg, AnnProgDecl, AnnStmtSeq (..), mkAnnProgDecl)
import qualified Gameverif.Viper.Parser as VP
import SimpleParser (MatchBlock (..), MatchCase (..), anyToken, betweenParser, greedyStarParser, lookAheadMatch,
                     sepByParser)

type EcsyExp = AnnExp Extent
type EcsyStmtSeq = AnnStmtSeq Extent
type EcsyProgDecl a = AnnProgDecl Extent a
type EcsyProg a = AnnProg Extent a

funcP :: ParserM (FuncDecl VP.VipExp EcsyStmtSeq Text)
funcP = error "TODO"

resP :: ParserM (ResDecl VP.VipExp Text)
resP = error "TODO"

compP :: ParserM CompDecl
compP = error "TODO"

archP :: ParserM ArchDecl
archP = error "TODO"

queryNP :: ParserM QueryName
queryNP = fmap QueryName identP

compNP :: ParserM CompName
compNP = fmap CompName identP

queryAttrP :: ParserM QueryAttr
queryAttrP = yesMut <|> noMut where
  yesMut = keywordP "mut" *> fmap (`QueryAttr` AccessMut) compNP
  noMut = fmap (`QueryAttr` AccessConst) compNP

queryP :: ParserM QueryDecl
queryP = lexP $ do
  keywordP "query"
  name <- queryNP
  attrs <- betweenParser openBraceP closeBraceP (sepByParser queryAttrP commaP)
  let x = QueryDecl name attrs
  pure x

invP :: ParserM (InvDecl VP.VipExp Text)
invP = error "TODO"

sysP :: ParserM (SysDecl VP.VipExp EcsyStmtSeq Text)
sysP = error "TODO"

mainP :: ParserM (MainDecl VP.VipExp EcsyStmtSeq Text)
mainP = error "TODO"

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
