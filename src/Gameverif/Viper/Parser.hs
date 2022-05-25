{-# LANGUAGE OverloadedStrings #-}

{-
DEBUGGING:
make ghci
import qualified Easychair.Parsers.Viper as V
import Text.Pretty.Simple (pPrint)
parseVipProgramFileInteractive "test.vpr" >>= pPrint
debugVipInteractive V.expP "0"
-}
module Gameverif.Viper.Parser where

import Control.Applicative (empty, (<|>))
import Control.Monad (guard, void, (>=>))
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (gets)
import Data.Char (isAlpha, isDigit)
import Data.Foldable (asum, toList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Gameverif.Viper.Base (Action (..), ArgDecl (..), AxName (..), AxiomDecl (..), BuiltOp (..), DomDecl (..),
                                DomFuncDecl (..), ExpF (..), FieldDecl (..), FieldName (..), Fixity (..), FuncDecl (..),
                                FuncName (..), Lit (..), LitTy (..), Local (..), MethDecl (..), MethName (..), Op (..),
                                PredDecl (..), PredName (..), ProgDecl (..), ProofAction (..), Quant (..),
                                QuantVar (..), StmtF (..), StmtSeqF (..), Ty (..), TyName (..), VarName (..), opFixity)
import Gameverif.Viper.Concrete (AnnExp (..), AnnProg, AnnProgDecl, AnnStmtSeq (..), mkAnnExp, mkAnnProgDecl,
                                    mkAnnStmtSeq)
import Gameverif.Util.Console (ConsoleEnv (ConsoleEnv), ConsoleM)
import Errata (Block, PointerStyle, Style)
import SimpleParser (Chunked (..), EmbedTextLabel (..), ExplainLabel (..), LexedSpan (..), LexedStream (..), LinePos,
                     MatchBlock (..), MatchCase (..), ParseErrorBundle (..), ParseResult (..), ParseSuccess (..),
                     Parser, PosStream (..), ShowTextBuildable (..), Span (..), Spanned (..), Stream (..),
                     TextBuildable, TextLabel, TextualChunked (..), TextualStream, anyToken, applySign, betweenParser,
                     dropTokensWhile, dropTokensWhile1, escapedStringParser, greedyPlusParser, greedyStarParser,
                     lexedParseInteractive, lexedParser, lexemeParser, lookAheadMatch, markParser, matchChunk, matchEnd,
                     matchToken, newLinePosStream, numParser, optionalParser, parseInteractive, peekToken, popChunk,
                     popToken, runParser, satisfyToken, sepByParser, signParser, signedNumStartPred, takeTokensWhile,
                     takeTokensWhile1)
import SimpleParser.Errata (errataParseError)
import qualified Text.Builder as TB

-- Lexing:

data Atom =
    AtomIdent !Text
  | AtomBuiltOp !BuiltOp
  | AtomString !Text
  | AtomInt !Integer
  deriving stock (Eq, Show)
  deriving (TextBuildable) via (ShowTextBuildable Atom)

data VipTok =
    VipTokAtom !Atom
  | VipTokComma
  | VipTokPeriod
  | VipTokQuestion
  | VipTokColon
  | VipTokDoubleColon
  | VipTokColonEquals
  | VipTokOpenBrace
  | VipTokCloseBrace
  | VipTokOpenBracket
  | VipTokCloseBracket
  | VipTokOpenParen
  | VipTokCloseParen
  | VipTokNewline
  | VipTokComment !(Seq Text)
  deriving stock (Eq, Show)
  deriving (TextBuildable) via (ShowTextBuildable VipTok)

data VipTokLabel =
    VipTokLabelIdentStart
  | VipTokLabelBuiltOpStart
  | VipTokLabelEmbedText !TextLabel
  deriving stock (Eq, Show)

instance ExplainLabel VipTokLabel where
  explainLabel sl =
    case sl of
      VipTokLabelIdentStart -> "start of identifier"
      VipTokLabelBuiltOpStart -> "start of built-in op"
      VipTokLabelEmbedText tl -> explainLabel tl

instance EmbedTextLabel VipTokLabel where
  embedTextLabel = VipTokLabelEmbedText

type VipTokParserC s = (PosStream s, TextualStream s, Chunk s ~ Text)

type VipTokParserM s a = Parser VipTokLabel s Void a

spacePred :: Char -> Bool
spacePred c = c == ' ' || c == '\t'

lineSepPred :: Char -> Bool
lineSepPred c = c == '\n' || c == '\r'

puncPred :: Char -> Bool
puncPred c =
  c == '(' ||
  c == ')' ||
  c == '{' ||
  c == '}' ||
  c == '[' ||
  c == ']' ||
  c == ':' ||
  c == ';' ||
  c == ',' ||
  c == '.' ||
  c == '?'

commentPred :: Text -> Bool
commentPred = (== "//")

puncOrLineSepPred :: Char -> Bool
puncOrLineSepPred c = puncPred c || lineSepPred c

nonDelimPred :: Char -> Bool
nonDelimPred c = not (puncPred c) && not (spacePred c) && not (lineSepPred c) && c /= '/'

identStartPred :: Char -> Bool
identStartPred c = isAlpha c && identContPred c

identContPred :: Char -> Bool
identContPred = nonDelimPred

builtOpPred :: Char -> Bool
builtOpPred c =
  c == '-' ||
  c == '+' ||
  c == '=' ||
  c == '>' ||
  c == '<' ||
  c == '&' ||
  c == '|' ||
  c == '/' ||
  c == '%' ||
  c == '!' ||
  c == '*'

stringTP :: VipTokParserC s => VipTokParserM s Text
stringTP = fmap packChunk (escapedStringParser '"')

identifierTP :: VipTokParserC s => VipTokParserM s Text
identifierTP = do
  x <- satisfyToken (Just VipTokLabelIdentStart) identStartPred
  xs <- takeTokensWhile identContPred
  pure (packChunk (consChunk x xs))

spaceTP :: VipTokParserC s => VipTokParserM s ()
spaceTP = void (dropTokensWhile spacePred)

lexTP :: VipTokParserC s => VipTokParserM s a -> VipTokParserM s a
lexTP = lexemeParser spaceTP

openParenTP, closeParenTP, openBraceTP, closeBraceTP, openBracketTP, closeBracketTP :: VipTokParserC s => VipTokParserM s VipTok
openParenTP = VipTokOpenParen <$ lexTP (void (matchToken '('))
closeParenTP = VipTokCloseParen <$ lexTP (void (matchToken ')'))
openBraceTP = VipTokOpenBrace <$ lexTP (void (matchToken '{'))
closeBraceTP = VipTokCloseBrace <$ lexTP (void (matchToken '}'))
openBracketTP = VipTokOpenBracket <$ lexTP (void (matchToken '['))
closeBracketTP = VipTokCloseBracket <$ lexTP (void (matchToken ']'))

colonTP, doubleColonTP, colonEqualsTP :: VipTokParserC s => VipTokParserM s VipTok
colonTP = VipTokColon <$ do
  lexTP $ do
    void (matchToken ':')
    t <- peekToken
    guard (t /= Just ':' && t /= Just '=')
doubleColonTP = VipTokDoubleColon <$ lexTP (void (matchChunk "::"))
colonEqualsTP = VipTokColonEquals <$ lexTP (void (matchChunk ":="))

commaTP, questionTP, newlineTP, semicolonTP :: VipTokParserC s => VipTokParserM s VipTok
commaTP = VipTokComma <$ lexTP (void (matchToken ','))
questionTP = VipTokQuestion <$ lexTP (void (matchToken '?'))
newlineTP = VipTokNewline <$ lexTP (void (dropTokensWhile1 Nothing lineSepPred))
semicolonTP = VipTokNewline <$ lexTP (void (matchToken ';'))

singleCommentTP :: VipTokParserC s => VipTokParserM s Text
singleCommentTP = lexTP $ do
  void (matchChunk "//")
  comment <- takeTokensWhile (not . lineSepPred)
  void (satisfyToken Nothing lineSepPred)
  pure comment

commentTP :: VipTokParserC s => VipTokParserM s VipTok
commentTP = VipTokComment <$> greedyPlusParser singleCommentTP

puncOrLineSepTP :: VipTokParserC s => VipTokParserM s VipTok
puncOrLineSepTP = asum
  [ openParenTP, closeParenTP, openBraceTP, closeBraceTP, openBracketTP, closeBracketTP
  , doubleColonTP, colonEqualsTP, colonTP, commaTP, questionTP, newlineTP, semicolonTP
  ]

numAtomTP :: VipTokParserC s => VipTokParserM s Atom
numAtomTP = do
  ms <- signParser
  n <- numParser
  case n of
    Left i -> pure (AtomInt (applySign ms i))
    Right _ -> fail "floats not supported"

chunk1 :: VipTokParserC s => VipTokParserM s Text
chunk1 = do
  mc <- popChunk 2
  case mc of
    Just c | not (chunkEmpty c) -> pure (packChunk c)
    _ -> empty

unaryIdentPred :: Char -> Text -> Bool
unaryIdentPred u t0 =
  case T.uncons t0 of
    Just (c0, t1) | u == c0 ->
      case T.uncons t1 of
        Just (c1, _) -> not (isDigit c1)
        Nothing -> True
    _ -> False

opTP :: VipTokParserC s => VipTokParserM s BuiltOp
opTP = do
  chunk <- takeTokensWhile1 (Just VipTokLabelIdentStart) builtOpPred
  case chunk of
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
    _ -> fail ("invalid op: " <> T.unpack chunk)

identAtomTP :: VipTokParserC s => VipTokParserM s Atom
identAtomTP = fmap AtomIdent identifierTP

atomTP :: VipTokParserC s => VipTokParserM s Atom
atomTP = lexTP (lookAheadMatch block) where
  block = MatchBlock chunk1 (fail "failed to parse sexp atom")
    [ MatchCase Nothing ((== '"') . T.head) (fmap AtomString stringTP)
    , MatchCase Nothing (unaryIdentPred '+') (fmap AtomBuiltOp opTP)
    , MatchCase Nothing (unaryIdentPred '-') (fmap AtomBuiltOp opTP)
    , MatchCase Nothing (signedNumStartPred . T.head) numAtomTP
    , MatchCase Nothing (builtOpPred . T.head) (fmap AtomBuiltOp opTP)
    , MatchCase Nothing (identStartPred . T.head) identAtomTP
    ]

vipTokParser :: VipTokParserC s => VipTokParserM s VipTok
vipTokParser = lookAheadMatch block where
  block = MatchBlock chunk1 (fmap VipTokAtom atomTP)
    [ MatchCase Nothing commentPred commentTP
    , MatchCase Nothing (puncOrLineSepPred . T.head) puncOrLineSepTP
    ]

vipTokStreamParser :: VipTokParserC s => VipTokParserM s (Seq VipTok)
vipTokStreamParser = greedyStarParser vipTokParser

parseVipTokStreamInteractive :: String -> IO (Maybe (Seq VipTok))
parseVipTokStreamInteractive = parseInteractive vipTokStreamParser

parseVipTokStreamFileInteractive :: FilePath -> IO (Maybe (Seq VipTok))
parseVipTokStreamFileInteractive = readFile >=> parseVipTokStreamInteractive

filterNonComments :: (x -> VipTok) -> Seq x -> Seq x
filterNonComments p = Seq.filter ((\case { VipTokComment _ -> False; _ -> True }) . p)

dropLeadingNewlines :: (x -> VipTok) -> Seq x -> Seq x
dropLeadingNewlines p = Seq.dropWhileL ((\case { VipTokNewline -> True; _ -> False }) . p)

dedupNewlines :: (x -> VipTok) -> Seq x -> Seq x
dedupNewlines p = go Empty Nothing where
  go !acc !mx = \case
    Empty -> maybe acc (acc :|>) mx
    y :<| ys ->
      case mx of
        Nothing -> go acc (Just y) ys
        Just x ->
          if p y == VipTokNewline && p x == VipTokNewline
            then go acc mx ys
            else go (acc :|> x) (Just y) ys

xformStream :: LexedStream p VipTok -> LexedStream p VipTok
xformStream (LexedStream ss ep) = LexedStream (f ss) ep where
  p = spannedValue
  f = dedupNewlines p . dropLeadingNewlines p . filterNonComments p

-- Parsing:

data VipLabel =
    VipLabelKeyword !Text
  | VipLabelNewline
  | VipLabelIdent
  | VipLabelBuiltOp
  | VipLabelInt
  | VipLabelComma
  | VipLabelPeriod
  | VipLabelQuestion
  | VipLabelColon
  | VipLabelDoubleColon
  | VipLabelColonEquals
  | VipLabelOpenParen
  | VipLabelCloseParen
  | VipLabelOpenBrace
  | VipLabelCloseBrace
  | VipLabelOpenBracket
  | VipLabelCloseBracket
  deriving stock (Eq, Show)

instance ExplainLabel VipLabel where
  explainLabel = \case
    VipLabelKeyword t -> "keyword " <> TB.text t
    VipLabelNewline -> "newline/semicolon"
    VipLabelIdent -> "identifier"
    VipLabelBuiltOp -> "built-in op"
    VipLabelInt -> "integer"
    VipLabelComma -> "comma"
    VipLabelPeriod -> "period"
    VipLabelQuestion -> "question"
    VipLabelColon -> "colon"
    VipLabelDoubleColon -> "double colon"
    VipLabelColonEquals -> "colon equals"
    VipLabelOpenParen -> "open paren"
    VipLabelCloseParen -> "close paren"
    VipLabelOpenBrace -> "open brace"
    VipLabelCloseBrace -> "close brace"
    VipLabelOpenBracket -> "open bracket"
    VipLabelCloseBracket -> "close bracket"

type VipPos p = Span p
type VipExp p = AnnExp (VipPos p)
type VipStmtSeq p = AnnStmtSeq (VipPos p)
type VipProgDecl p a = AnnProgDecl (VipPos p) a

type VipParserC p s = (PosStream s, Pos s ~ LexedSpan p, Token s ~ VipTok, Chunk s ~ Seq VipTok)

type VipParserM s a = Parser VipLabel s Void a

startLexed :: LexedSpan p -> p
startLexed = \case
  LexedSpanElem s -> spanStart s
  LexedSpanEnd p -> p

spanLexed :: LexedSpan p -> LexedSpan p -> VipPos p
spanLexed ls0 ls1 = Span (startLexed ls0) (startLexed ls1)

withVipPos :: VipParserC p s => (VipPos p -> a -> b) -> VipParserM s a -> VipParserM s b
withVipPos f m = do
  ls0 <- gets streamViewPos
  a <- m
  ls1 <- gets streamViewPos
  let s = spanLexed ls0 ls1
      b = f s a
  pure b

tokSpacePred, newlinePred, commaPred, periodPred, questionPred, colonPred, colonEqualsPred, doubleColonPred, openParenPred,
  closeParenPred, openBracePred, closeBracePred, openBracketPred, closeBracketPred :: VipTok -> Bool
tokSpacePred = \case
  VipTokNewline -> True
  VipTokComment _ -> True
  _ -> False
newlinePred = \case { VipTokNewline -> True; _ -> False }
commaPred = \case { VipTokComma -> True; _ -> False }
questionPred = \case { VipTokQuestion -> True; _ -> False }
colonPred = \case { VipTokColon -> True; _ -> False }
periodPred = \case { VipTokPeriod -> True; _ -> False }
doubleColonPred = \case { VipTokDoubleColon -> True; _ -> False }
colonEqualsPred = \case { VipTokColonEquals -> True; _ -> False }
openParenPred = \case { VipTokOpenParen -> True; _ -> False }
closeParenPred = \case { VipTokCloseParen -> True; _ -> False }
openBracePred = \case { VipTokOpenBrace -> True; _ -> False }
closeBracePred = \case { VipTokCloseBrace -> True; _ -> False }
openBracketPred = \case { VipTokOpenBracket -> True; _ -> False }
closeBracketPred = \case { VipTokCloseBracket -> True; _ -> False }

keywordPred :: Text -> VipTok -> Bool
keywordPred t = \case
  VipTokAtom (AtomIdent s) | s == t -> True
  _ -> False

spaceP, newlineP :: VipParserC p s => VipParserM s ()
spaceP = void (dropTokensWhile tokSpacePred)
newlineP = void (satisfyToken (Just VipLabelNewline) newlinePred)

lexP :: VipParserC p s => VipParserM s a -> VipParserM s a
lexP = lexemeParser spaceP

rawCommaP, rawPeriodP, rawQuestionP, rawColonP, rawDoubleColonP, rawColonEqualsP, rawOpenParenP, rawCloseParenP, rawOpenBraceP, rawCloseBraceP, rawOpenBracketP, rawCloseBracketP :: VipParserC p s => VipParserM s ()
rawCommaP = void (satisfyToken (Just VipLabelComma) commaPred)
rawPeriodP = void (satisfyToken (Just VipLabelPeriod) periodPred)
rawQuestionP = void (satisfyToken (Just VipLabelQuestion) questionPred)
rawColonP = void (satisfyToken (Just VipLabelColon) colonPred)
rawDoubleColonP = void (satisfyToken (Just VipLabelDoubleColon) doubleColonPred)
rawColonEqualsP = void (satisfyToken (Just VipLabelColonEquals) colonEqualsPred)
rawOpenParenP = void (satisfyToken (Just VipLabelOpenParen) openParenPred)
rawCloseParenP = void (satisfyToken (Just VipLabelCloseParen) closeParenPred)
rawOpenBraceP = void (satisfyToken (Just VipLabelOpenBrace) openBracePred)
rawCloseBraceP = void (satisfyToken (Just VipLabelCloseBrace) closeBracePred)
rawOpenBracketP = void (satisfyToken (Just VipLabelOpenBracket) openBracketPred)
rawCloseBracketP = void (satisfyToken (Just VipLabelCloseBracket) closeBracketPred)

commaP, periodP, questionP, colonP, doubleColonP, colonEqualsP, openParenP, closeParenP, openBraceP, closeBraceP, openBracketP, closeBracketP :: VipParserC p s => VipParserM s ()
commaP = lexP rawCommaP
periodP = lexP rawPeriodP
questionP = lexP rawQuestionP
colonP = lexP rawColonP
doubleColonP = lexP rawDoubleColonP
colonEqualsP = lexP rawColonEqualsP
openParenP = lexP rawOpenParenP
closeParenP = lexP rawCloseParenP
openBraceP = lexP rawOpenBraceP
closeBraceP = lexP rawCloseBraceP
openBracketP = lexP rawOpenBracketP
closeBracketP = lexP rawCloseBracketP

rawIdentP, identP :: VipParserC p s => VipParserM s Text
rawIdentP = markParser (Just VipLabelIdent) $ do
  tok <- anyToken
  case tok of
    VipTokAtom (AtomIdent s) -> pure s
    _ -> fail "expected identifier"
identP = lexP rawIdentP

rawIntP, intP :: VipParserC p s => VipParserM s Integer
rawIntP = markParser (Just VipLabelInt) $ do
  tok <- anyToken
  case tok of
    VipTokAtom (AtomInt i) -> pure i
    _ -> fail "expected integer"
intP = lexP rawIntP

rawKeywordP, keywordP :: VipParserC p s => Text -> VipParserM s ()
rawKeywordP t = void (satisfyToken (Just (VipLabelKeyword t)) (keywordPred t))
keywordP = rawKeywordP

tyP :: VipParserC p s => VipParserM s Ty
tyP = identP >>= onIdent where
  onIdent = \case
    "Int" -> pure (TyLit LitTyInt)
    "Bool" -> pure (TyLit LitTyBool)
    other -> pure (TyDomain (TyName other))

prefixBuiltOpP :: VipParserC p s => VipParserM s BuiltOp
prefixBuiltOpP = lexP $ popToken >>= \case
  Just (VipTokAtom (AtomBuiltOp op)) ->
    if opFixity op == FixityPre
      then pure op
      else fail "invalid prefix built op (not prefix)"
  _ -> fail "invalid prefix built op (not built op)"

infixBuiltOpP :: VipParserC p s => VipParserM s BuiltOp
infixBuiltOpP = lexP $ popToken >>= \case
  Just (VipTokAtom (AtomBuiltOp op)) ->
    if opFixity op == FixityIn
      then pure op
      else fail "invalid infix built op (not infix)"
  _ -> fail "invalid infix built op (not built op)"

prefixOpP :: VipParserC p s => VipParserM s (Op Text)
prefixOpP = OpBuilt <$> prefixBuiltOpP <|> OpFree <$> identP

expP :: VipParserC p s => VipParserM s (VipExp p Text)
expP = recExpP where
  recExpP = do
    e <- baseExpP
    contExpP e <|> pure e
  baseExpP = asum [expParenP, expTrueP, expFalseP, expIntP, expQuantP, expPrefixAppP, expOldP, expVarP]
  contExpP e = asum [contCondExpP e, contInfixAppP e, contFieldExpP e]
  tieExpP = withVipPos mkAnnExp
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
  expParenP = betweenParser openParenP closeParenP recExpP
  expTrueP = tieExpP $ ExpLitF (LitBool True) <$ keywordP "true"
  expFalseP = tieExpP $ ExpLitF (LitBool False) <$ keywordP "false"
  expIntP = tieExpP $ ExpLitF . LitInt <$> intP
  expVarP = tieExpP $ ExpVarF <$> identP
  expPrefixAppP = tieExpP $ do
    op <- prefixOpP
    args <- betweenParser openParenP closeParenP (sepByParser recExpP commaP)
    pure (ExpAppF op args)
  expOldP = tieExpP $ do
    keywordP "old"
    ExpOldF <$> optionalParser (betweenParser openBracketP closeBracketP identP) <*> betweenParser openParenP closeParenP recExpP
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
  triggerP = betweenParser openBraceP closeBraceP (sepByParser recExpP commaP)

actionP :: VipParserC p s => VipParserM s (Action (VipExp p) Text)
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

localP :: VipParserC p s => VipParserM s (Local (VipExp p) Text)
localP = do
  keywordP "var"
  name <- identP
  colonP
  ty <- tyP
  me <- do
    z <- peekToken
    case z of
      Just VipTokColonEquals -> do
        colonEqualsP
        Just <$> expP
      _ -> pure Nothing
  let x = Local (VarName name) ty me
  pure x

stmtP :: VipParserC p s => VipParserM s a -> VipParserM s (StmtF (VipExp p) Text a)
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
    g <- betweenParser openParenP closeParenP expP
    t <- betweenParser openBraceP closeBraceP p
    me <- do
      z <- peekToken
      case z of
        Just (VipTokAtom (AtomIdent "else")) -> do
          keywordP "else"
          Just <$> betweenParser openBraceP closeBraceP p
        _ -> pure Nothing
    let x = StmtIfF g t me
    pure x
  invariantP = keywordP "invariant" *> expP
  stmtWhileP = do
    keywordP "while"
    g <- betweenParser openParenP closeParenP expP
    is <- greedyStarParser invariantP
    body <- betweenParser openBraceP closeBraceP p
    let x = StmtWhileF g is body
    pure x

stmtSeqP :: VipParserC p s => VipParserM s (VipStmtSeq p Text)
stmtSeqP = recP where
  recP = withVipPos mkAnnStmtSeq (consP <|> nilP)
  consP = StmtSeqConsF <$> stmtP recP <*> recP
  nilP = lexP $ pure StmtSeqNilF

argP :: VipParserC p s => VipParserM s ArgDecl
argP = do
  name <- identP
  colonP
  ty <- tyP
  let x = ArgDecl (VarName name) ty
  pure x

argsP :: VipParserC p s => VipParserM s (Seq ArgDecl)
argsP = sepByParser argP commaP

fieldP :: VipParserC p s => VipParserM s FieldDecl
fieldP = lexP $ do
  keywordP "field"
  name <- identP
  colonP
  ty <- tyP
  let x = FieldDecl (FieldName name) ty
  pure x

returnsP :: VipParserC p s => VipParserM s (Seq ArgDecl)
returnsP = do
  keywordP "returns"
  betweenParser openParenP closeParenP argsP

requiresP, ensuresP :: VipParserC p s => VipParserM s (VipExp p Text)
requiresP = rawKeywordP "requires" *> expP
ensuresP = rawKeywordP "ensures" *> expP

methP :: VipParserC p s => VipParserM s (MethDecl (VipExp p) (VipStmtSeq p) Text)
methP = lexP $ do
  keywordP "method"
  name <- identP
  args <- betweenParser openParenP closeParenP argsP
  rets <- returnsP <|> pure Empty
  reqs <- greedyStarParser requiresP
  ens <- greedyStarParser ensuresP
  body <- betweenParser openBraceP closeBraceP stmtSeqP
  let x = MethDecl (MethName name) args rets reqs ens body
  pure x

predP :: VipParserC p s => VipParserM s (PredDecl (VipExp p) Text)
predP = lexP $ do
  keywordP "predicate"
  name <- identP
  args <- betweenParser openParenP closeParenP argsP
  body <- betweenParser openBraceP closeBraceP expP
  let x = PredDecl (PredName name) args body
  pure x

domFuncP :: VipParserC p s => VipParserM s (DomFuncDecl Text)
domFuncP = lexP $ do
  keywordP "function"
  name <- identP
  args <- betweenParser openParenP closeParenP argsP
  colonP
  ty <- tyP
  let x = DomFuncDecl (FuncName name) args ty
  pure x

funcP :: VipParserC p s => VipParserM s (FuncDecl (VipExp p) Text)
funcP = lexP $ do
  keywordP "function"
  name <- identP
  args <- betweenParser openParenP closeParenP argsP
  colonP
  ty <- tyP
  reqs <- greedyStarParser requiresP
  ens <- greedyStarParser ensuresP
  body <- betweenParser openBraceP closeBraceP expP
  let x = FuncDecl (FuncName name) args ty reqs ens body
  pure x

domP :: VipParserC p s => VipParserM s (DomDecl (VipExp p) Text)
domP = lexP $ do
  keywordP "domain"
  name <- identP
  elts <- betweenParser openBraceP closeBraceP (greedyStarParser vipDomEltParser)
  let (funcs, axioms) = splitDomElts elts
      x = DomDecl (TyName name) funcs axioms
  pure x

axiomP :: VipParserC p s => VipParserM s (AxiomDecl (VipExp p) Text)
axiomP = lexP $ do
  keywordP "axiom"
  man <- optionalParser (AxName <$> identP)
  body <- betweenParser openBraceP closeBraceP expP
  let x = AxiomDecl man body
  pure x

data DomElt e v =
    DomEltFunc !(DomFuncDecl v)
  | DomEltAxiom !(AxiomDecl e v)
  deriving stock (Eq, Show)

splitDomElts :: Seq (DomElt e v) -> (Seq (DomFuncDecl v), Seq (AxiomDecl e v))
splitDomElts = go Empty Empty where
  go !funAcc !axAcc = \case
    Empty -> (funAcc, axAcc)
    elt :<| elts ->
      case elt of
        DomEltFunc fd -> go (funAcc :|> fd) axAcc elts
        DomEltAxiom ad -> go funAcc (axAcc :|> ad) elts

vipDomEltParser :: VipParserC p s => VipParserM s (DomElt (VipExp p) Text)
vipDomEltParser = lookAheadMatch block where
  block = MatchBlock anyToken (fail "invalid domain element")
    [ MatchCase Nothing (keywordPred "function") (fmap DomEltFunc domFuncP)
    , MatchCase Nothing (keywordPred "axiom") (fmap DomEltAxiom axiomP)
    ]

vipDeclParser :: VipParserC p s => VipParserM s (AnnProgDecl (VipPos p) Text)
vipDeclParser = withVipPos mkAnnProgDecl (lookAheadMatch block) where
  block = MatchBlock anyToken (fail "invalid declaration")
    [ MatchCase Nothing (keywordPred "field") (fmap ProgDeclField fieldP)
    , MatchCase Nothing (keywordPred "domain") (fmap ProgDeclDom domP)
    , MatchCase Nothing (keywordPred "function") (fmap ProgDeclFunc funcP)
    , MatchCase Nothing (keywordPred "method") (fmap ProgDeclMeth methP)
    , MatchCase Nothing (keywordPred "predicate") (fmap ProgDeclPred predP)
    ]

vipProgramParser :: VipParserC p s => VipParserM s (AnnProg (VipPos p) Text)
vipProgramParser = greedyStarParser vipDeclParser

debugVipInteractive :: VipParserM (LexedStream LinePos VipTok) a -> String -> IO (Maybe a)
debugVipInteractive = lexedParseInteractive vipTokParser xformStream

parseVipProgramInteractive :: String -> IO (Maybe (AnnProg (Span LinePos) Text))
parseVipProgramInteractive = lexedParseInteractive vipTokParser xformStream vipProgramParser

parseVipProgramFileInteractive :: FilePath -> IO (Maybe (AnnProg (Span LinePos) Text))
parseVipProgramFileInteractive = readFile >=> parseVipProgramInteractive

data VipParseResult a =
    VPRLexErr ![Block]
  | VPRLexEmpty
  | VPRParseErr ![Block]
  | VPRParseEmpty
  | VPRParseSuccess !a
  deriving stock (Show)

parseVipProgramErrata :: Style -> PointerStyle -> FilePath -> Text -> VipParseResult (AnnProg (Span LinePos) Text)
parseVipProgramErrata bsty psty fp contents =
  let mkBlocksLex = fmap (errataParseError bsty psty fp) . toList
      mkBlocksParse = fmap (errataParseError bsty psty fp) . toList
      lexRes = runParser (lexedParser vipTokParser <* matchEnd) (newLinePosStream contents)
  in case lexRes of
    Just (ParseResultSuccess (ParseSuccess _ ls)) ->
      let parseRes = runParser (vipProgramParser <* matchEnd) (xformStream ls)
      in case parseRes of
        Just (ParseResultSuccess (ParseSuccess _ a)) -> VPRParseSuccess a
        Just (ParseResultError (ParseErrorBundle errs)) -> VPRParseErr (mkBlocksLex errs)
        Nothing -> VPRParseEmpty
    Just (ParseResultError (ParseErrorBundle errs)) -> VPRLexErr (mkBlocksParse errs)
    Nothing -> VPRLexEmpty

parseVipProgramConsole :: ConsoleM (VipParseResult (AnnProg (Span LinePos) Text))
parseVipProgramConsole = do
  ConsoleEnv bsty psty fp contents <- ask
  pure (parseVipProgramErrata bsty psty fp contents)
