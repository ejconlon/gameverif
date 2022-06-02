{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Ecsy.Parser where

import Data.Text (Text)
import Gameverif.Common.Parser (Extent, ParserM, keywordPred, withPos)
import Gameverif.Ecsy.Base (ArchDecl, CompDecl, FuncDecl, InvDecl, MainDecl, ProgDecl (..), QueryDecl, ResDecl, SysDecl)
import Gameverif.Ecsy.Concrete (AnnExp (..), AnnProg, AnnProgDecl, AnnStmtSeq (..), mkAnnProgDecl)
import qualified Gameverif.Viper.Parser as VP
import SimpleParser (MatchBlock (..), MatchCase (..), anyToken, greedyStarParser, lookAheadMatch)

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

queryP :: ParserM QueryDecl
queryP = error "TODO"

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
