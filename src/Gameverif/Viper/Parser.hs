{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Viper.Parser where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Gameverif.Common.Lexer (Atom (..), Tok (..))
import Gameverif.Common.Parser (Extent, ParserM, betweenBracesP, betweenBracketsP, betweenParensP, colonEqualsP, colonP,
                                commaP, doubleColonP, identP, intP, keywordP, keywordPred, lexP, periodP, questionP,
                                rawKeywordP, withPos)
import Gameverif.Viper.Base (Action (..), ArgDecl (..), AxName (..), AxiomDecl (..), BuiltOp (..), DomDecl (..),
                             DomFuncDecl (..), ExpF (..), FieldDecl (..), FieldName (..), Fixity (..), FuncDecl (..),
                             FuncName (..), Lit (..), LitTy (..), Local (..), MethDecl (..), MethName (..), Op (..),
                             PredDecl (..), PredName (..), ProgDecl (..), ProofAction (..), Quant (..), QuantVar (..),
                             StmtF (..), StmtSeqF (..), Ty (..), TyName (..), VarName (..), opFixity)
import Gameverif.Viper.Concrete (AnnExp (..), AnnProg, AnnProgDecl, AnnStmtSeq (..), mkAnnExp, mkAnnProgDecl,
                                 mkAnnStmtSeq)
import SimpleParser (MatchBlock (..), MatchCase (..), anyToken, greedyPlusParser, greedyStarParser, lookAheadMatch,
                     optionalParser, peekToken, popToken, sepByParser)

type VipExp = AnnExp Extent
type VipStmtSeq = AnnStmtSeq Extent
type VipProgDecl a = AnnProgDecl Extent a
type VipProg a = AnnProg Extent a

opP :: ParserM BuiltOp
opP = lexP $ popToken >>= \case
  Just (TokAtom (AtomBuiltOp op)) ->
    case op of
      "<==>" -> pure BuiltOpIff
      "==>" -> pure BuiltOpImplies
      "==" -> pure BuiltOpEquals
      "!=" -> pure BuiltOpNotEquals
      "--*" -> pure BuiltOpWand
      "&&" -> pure BuiltOpAnd
      "||" -> pure BuiltOpOr
      "!" -> pure BuiltOpNot
      "+" -> pure BuiltOpAdd
      "-" -> pure BuiltOpSub
      "*" -> pure BuiltOpMul
      "/" -> pure BuiltOpDiv
      "%" -> pure BuiltOpMod
      ">" -> pure BuiltOpGt
      ">=" -> pure BuiltOpGte
      "<" -> pure BuiltOpLt
      "<=" -> pure BuiltOpLte
      _ -> fail "invalid built op (unrecognized)"
  _ -> fail "invalid built op (not lexed)"

tyP :: ParserM Ty
tyP = identP >>= onIdent where
  onIdent = \case
    "Int" -> pure (TyLit LitTyInt)
    "Bool" -> pure (TyLit LitTyBool)
    other -> pure (TyDomain (TyName other))

prefixBuiltOpP :: ParserM BuiltOp
prefixBuiltOpP = do
  op <- opP
  if opFixity op == FixityPre
    then pure op
    else fail "invalid built op (not prefix)"

infixBuiltOpP :: ParserM BuiltOp
infixBuiltOpP = do
  op <- opP
  if opFixity op == FixityIn
    then pure op
    else fail "invalid built op (not infix)"

prefixOpP :: ParserM (Op Text)
prefixOpP = OpBuilt <$> prefixBuiltOpP <|> OpFree <$> identP

expP :: ParserM (VipExp Text)
expP = recExpP where
  recExpP = do
    e <- baseExpP
    contExpP e <|> pure e
  baseExpP = asum [expParenP, expTrueP, expFalseP, expIntP, expQuantP, expPrefixAppP, expOldP, expVarP]
  contExpP e = asum [contCondExpP e, contInfixAppP e, contFieldExpP e]
  tieExpP = withPos mkAnnExp
  contCondExpP g = tieExpP $ do
    questionP
    t <- recExpP
    colonP
    e <- recExpP
    let x = ExpCondF g t e
    pure x
  contInfixAppP g = tieExpP $ do
    op <- infixBuiltOpP
    e <- recExpP
    let x = ExpAppF (OpBuilt op) (g :<| e :<| Empty)
    pure x
  contFieldExpP g = tieExpP $ do
    periodP
    n <- identP
    let x = ExpFieldF g (FieldName n)
    pure x
  expParenP = betweenParensP recExpP
  expTrueP = tieExpP $ ExpLitF (LitBool True) <$ keywordP "true"
  expFalseP = tieExpP $ ExpLitF (LitBool False) <$ keywordP "false"
  expIntP = tieExpP $ ExpLitF . LitInt <$> intP
  expVarP = tieExpP $ ExpVarF <$> identP
  expPrefixAppP = tieExpP $ do
    op <- prefixOpP
    args <- betweenParensP (sepByParser recExpP commaP)
    pure (ExpAppF op args)
  expOldP = tieExpP $ do
    keywordP "old"
    ExpOldF <$> optionalParser (betweenBracketsP identP) <*> betweenParensP recExpP
  quantVarP = do
    vn <- identP
    colonP
    tp <- tyP
    let x = QuantVar (VarName vn) tp
    pure x
  expQuantP = tieExpP $ do
    quant <- (QuantForall <$ keywordP "forall") <|> (QuantExists <$ keywordP "exists")
    vs <- sepByParser quantVarP commaP
    guard (not (Seq.null vs))
    doubleColonP
    tgs <- greedyStarParser triggerP
    body <- recExpP
    let x = ExpQuantF quant vs tgs body
    pure x
  triggerP = betweenBracesP (sepByParser recExpP commaP)

actionP :: ParserM (Action VipExp Text)
actionP = baseActionP where
  baseActionP = asum
    [ actionInhaleP, actionExhaleP, actionAssertP, actionAssumeP
    , actionAssignVarsP, actionAssignFieldP
    ]
  actionExpP k = keywordP k *> expP
  actionInhaleP = ActionProof ProofActionInhale <$> actionExpP "inhale"
  actionExhaleP = ActionProof ProofActionExhale <$> actionExpP "exhale"
  actionAssertP = ActionProof ProofActionAssert <$> actionExpP "assert"
  actionAssumeP = ActionProof ProofActionAssume <$> actionExpP "assume"
  actionAssignVarsP = do
    vs <- sepByParser identP commaP
    guard (not (Seq.null vs))
    colonEqualsP
    e <- expP
    let x = ActionAssignVars vs e
    pure x
  actionAssignFieldP = do
    v <- identP
    periodP
    f <- identP
    colonEqualsP
    e <- expP
    let x = ActionAssignField v (FieldName f) e
    pure x

localP :: ParserM (Local VipExp Text)
localP = do
  keywordP "var"
  name <- identP
  colonP
  ty <- tyP
  me <- do
    z <- peekToken
    case z of
      Just TokColonEquals -> do
        colonEqualsP
        Just <$> expP
      _ -> pure Nothing
  let x = Local (VarName name) ty me
  pure x

stmtP :: ParserM a -> ParserM (StmtF VipExp Text a)
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
  invariantP = keywordP "invariant" *> expP
  stmtWhileP = do
    keywordP "while"
    g <- betweenParensP expP
    is <- greedyStarParser invariantP
    body <- betweenBracesP p
    let x = StmtWhileF g is body
    pure x

stmtSeqP :: ParserM (VipStmtSeq Text)
stmtSeqP = recP where
  recP = withPos mkAnnStmtSeq (consP <|> nilP)
  consP = StmtSeqConsF <$> stmtP recP <*> recP
  nilP = lexP $ pure StmtSeqNilF

argP :: ParserM ArgDecl
argP = do
  name <- identP
  colonP
  ty <- tyP
  let x = ArgDecl (VarName name) ty
  pure x

argsP :: ParserM (Seq ArgDecl)
argsP = sepByParser argP commaP

fieldP :: ParserM FieldDecl
fieldP = lexP $ do
  keywordP "field"
  name <- identP
  colonP
  ty <- tyP
  let x = FieldDecl (FieldName name) ty
  pure x

returnsP :: ParserM (Seq ArgDecl)
returnsP = do
  keywordP "returns"
  betweenParensP argsP

requiresP, ensuresP :: ParserM (VipExp Text)
requiresP = rawKeywordP "requires" *> expP
ensuresP = rawKeywordP "ensures" *> expP

methP :: ParserM (MethDecl VipExp VipStmtSeq Text)
methP = lexP $ do
  keywordP "method"
  name <- identP
  args <- betweenParensP argsP
  rets <- returnsP <|> pure Empty
  reqs <- greedyStarParser requiresP
  ens <- greedyStarParser ensuresP
  body <- betweenBracesP stmtSeqP
  let x = MethDecl (MethName name) args rets reqs ens body
  pure x

predP :: ParserM (PredDecl VipExp Text)
predP = lexP $ do
  keywordP "predicate"
  name <- identP
  args <- betweenParensP argsP
  body <- betweenBracesP expP
  let x = PredDecl (PredName name) args body
  pure x

domFuncP :: ParserM DomFuncDecl
domFuncP = lexP $ do
  keywordP "function"
  name <- identP
  args <- betweenParensP argsP
  colonP
  ty <- tyP
  let x = DomFuncDecl (FuncName name) args ty
  pure x

funcP :: ParserM (FuncDecl VipExp Text)
funcP = lexP $ do
  keywordP "function"
  name <- identP
  args <- betweenParensP argsP
  colonP
  ty <- tyP
  reqs <- greedyStarParser requiresP
  ens <- greedyStarParser ensuresP
  body <- betweenBracesP expP
  let x = FuncDecl (FuncName name) args ty reqs ens body
  pure x

domP :: ParserM (DomDecl VipExp Text)
domP = lexP $ do
  keywordP "domain"
  name <- identP
  elts <- betweenBracesP (greedyStarParser vipDomEltParser)
  let (funcs, axioms) = splitDomElts elts
      x = DomDecl (TyName name) funcs axioms
  pure x

axiomP :: ParserM (AxiomDecl VipExp Text)
axiomP = lexP $ do
  keywordP "axiom"
  man <- optionalParser (AxName <$> identP)
  body <- betweenBracesP expP
  let x = AxiomDecl man body
  pure x

data DomElt e v =
    DomEltFunc !DomFuncDecl
  | DomEltAxiom !(AxiomDecl e v)
  deriving stock (Eq, Show)

splitDomElts :: Seq (DomElt e v) -> (Seq DomFuncDecl, Seq (AxiomDecl e v))
splitDomElts = go Empty Empty where
  go !funAcc !axAcc = \case
    Empty -> (funAcc, axAcc)
    elt :<| elts ->
      case elt of
        DomEltFunc fd -> go (funAcc :|> fd) axAcc elts
        DomEltAxiom ad -> go funAcc (axAcc :|> ad) elts

vipDomEltParser :: ParserM (DomElt VipExp Text)
vipDomEltParser = lookAheadMatch block where
  block = MatchBlock anyToken (fail "invalid domain element")
    [ MatchCase Nothing (keywordPred "function") (fmap DomEltFunc domFuncP)
    , MatchCase Nothing (keywordPred "axiom") (fmap DomEltAxiom axiomP)
    ]

vipDeclParser :: ParserM (VipProgDecl Text)
vipDeclParser = withPos mkAnnProgDecl (lookAheadMatch block) where
  block = MatchBlock anyToken (fail "invalid declaration")
    [ MatchCase Nothing (keywordPred "field") (fmap ProgDeclField fieldP)
    , MatchCase Nothing (keywordPred "domain") (fmap ProgDeclDom domP)
    , MatchCase Nothing (keywordPred "function") (fmap ProgDeclFunc funcP)
    , MatchCase Nothing (keywordPred "method") (fmap ProgDeclMeth methP)
    , MatchCase Nothing (keywordPred "predicate") (fmap ProgDeclPred predP)
    ]

vipProgParser :: ParserM (VipProg Text)
vipProgParser = greedyStarParser vipDeclParser

vipExpParser :: ParserM (VipExp Text)
vipExpParser = expP
