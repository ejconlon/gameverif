{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Common.Lexer where

import Control.Applicative (empty)
import Control.Monad (guard, void, (>=>))
import Data.Char (isAlpha, isDigit)
import Data.Foldable (asum)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import SimpleParser (Chunked (..), EmbedTextLabel (..), ExplainLabel (..), LexedStream (..), LinePosStream,
                     MatchBlock (..), MatchCase (..), Parser, ShowTextBuildable (..), Spanned (..), TextBuildable,
                     TextLabel, TextualChunked (..), applySign, dropTokensWhile, dropTokensWhile1, escapedStringParser,
                     greedyPlusParser, greedyStarParser, lexemeParser, lookAheadMatch, matchChunk, matchToken,
                     numParser, parseInteractive, peekToken, popChunk, satisfyToken, signParser, signedNumStartPred,
                     takeTokensWhile, takeTokensWhile1)

data Atom =
    AtomIdent !Text
  | AtomBuiltOp !Text
  | AtomString !Text
  | AtomInt !Integer
  deriving stock (Eq, Show)
  deriving (TextBuildable) via (ShowTextBuildable Atom)

data Tok =
    TokAtom !Atom
  | TokComma
  | TokPeriod
  | TokQuestion
  | TokColon
  | TokDoubleColon
  | TokColonEquals
  | TokOpenBrace
  | TokCloseBrace
  | TokOpenBracket
  | TokCloseBracket
  | TokOpenParen
  | TokCloseParen
  | TokNewline
  | TokComment !(Seq Text)
  deriving stock (Eq, Show)
  deriving (TextBuildable) via (ShowTextBuildable Tok)

data TokLabel =
    TokLabelIdentStart
  | TokLabelBuiltOpStart
  | TokLabelEmbedText !TextLabel
  deriving stock (Eq, Show)

instance ExplainLabel TokLabel where
  explainLabel sl =
    case sl of
      TokLabelIdentStart -> "start of identifier"
      TokLabelBuiltOpStart -> "start of built-in op"
      TokLabelEmbedText tl -> explainLabel tl

instance EmbedTextLabel TokLabel where
  embedTextLabel = TokLabelEmbedText

type TokParserM a = Parser TokLabel (LinePosStream Text) Void a

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

stringTP :: TokParserM Text
stringTP = fmap packChunk (escapedStringParser '"')

identifierTP :: TokParserM Text
identifierTP = do
  x <- satisfyToken (Just TokLabelIdentStart) identStartPred
  xs <- takeTokensWhile identContPred
  pure (packChunk (consChunk x xs))

spaceTP :: TokParserM ()
spaceTP = void (dropTokensWhile spacePred)

lexTP :: TokParserM a -> TokParserM a
lexTP = lexemeParser spaceTP

openParenTP, closeParenTP, openBraceTP, closeBraceTP, openBracketTP, closeBracketTP :: TokParserM Tok
openParenTP = TokOpenParen <$ lexTP (void (matchToken '('))
closeParenTP = TokCloseParen <$ lexTP (void (matchToken ')'))
openBraceTP = TokOpenBrace <$ lexTP (void (matchToken '{'))
closeBraceTP = TokCloseBrace <$ lexTP (void (matchToken '}'))
openBracketTP = TokOpenBracket <$ lexTP (void (matchToken '['))
closeBracketTP = TokCloseBracket <$ lexTP (void (matchToken ']'))

colonTP, doubleColonTP, colonEqualsTP :: TokParserM Tok
colonTP = TokColon <$ do
  lexTP $ do
    void (matchToken ':')
    t <- peekToken
    guard (t /= Just ':' && t /= Just '=')
doubleColonTP = TokDoubleColon <$ lexTP (void (matchChunk "::"))
colonEqualsTP = TokColonEquals <$ lexTP (void (matchChunk ":="))

commaTP, questionTP, newlineTP, semicolonTP :: TokParserM Tok
commaTP = TokComma <$ lexTP (void (matchToken ','))
questionTP = TokQuestion <$ lexTP (void (matchToken '?'))
newlineTP = TokNewline <$ lexTP (void (dropTokensWhile1 Nothing lineSepPred))
semicolonTP = TokNewline <$ lexTP (void (matchToken ';'))

singleCommentTP :: TokParserM Text
singleCommentTP = lexTP $ do
  void (matchChunk "//")
  comment <- takeTokensWhile (not . lineSepPred)
  void (satisfyToken Nothing lineSepPred)
  pure comment

commentTP :: TokParserM Tok
commentTP = TokComment <$> greedyPlusParser singleCommentTP

puncOrLineSepTP :: TokParserM Tok
puncOrLineSepTP = asum
  [ openParenTP, closeParenTP, openBraceTP, closeBraceTP, openBracketTP, closeBracketTP
  , doubleColonTP, colonEqualsTP, colonTP, commaTP, questionTP, newlineTP, semicolonTP
  ]

numAtomTP :: TokParserM Atom
numAtomTP = do
  ms <- signParser
  n <- numParser
  case n of
    Left i -> pure (AtomInt (applySign ms i))
    Right _ -> fail "floats not supported"

chunk1 :: TokParserM Text
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

opTP :: TokParserM Text
opTP = do
  chunk <- takeTokensWhile1 (Just TokLabelIdentStart) builtOpPred
  case chunk of
    "<==>" -> pure chunk
    "==>" -> pure chunk
    "==" -> pure chunk
    "!=" -> pure chunk
    "--*" -> pure chunk
    "&&" -> pure chunk
    "||" -> pure chunk
    "!" -> pure chunk
    "+" -> pure chunk
    "-" -> pure chunk
    "*" -> pure chunk
    "/" -> pure chunk
    "%" -> pure chunk
    ">" -> pure chunk
    ">=" -> pure chunk
    "<" -> pure chunk
    "<=" -> pure chunk
    _ -> fail ("invalid op: " <> T.unpack chunk)

identAtomTP :: TokParserM Atom
identAtomTP = fmap AtomIdent identifierTP

atomTP :: TokParserM Atom
atomTP = lexTP (lookAheadMatch block) where
  block = MatchBlock chunk1 (fail "failed to parse sexp atom")
    [ MatchCase Nothing ((== '"') . T.head) (fmap AtomString stringTP)
    , MatchCase Nothing (unaryIdentPred '+') (fmap AtomBuiltOp opTP)
    , MatchCase Nothing (unaryIdentPred '-') (fmap AtomBuiltOp opTP)
    , MatchCase Nothing (signedNumStartPred . T.head) numAtomTP
    , MatchCase Nothing (builtOpPred . T.head) (fmap AtomBuiltOp opTP)
    , MatchCase Nothing (identStartPred . T.head) identAtomTP
    ]

tokParser :: TokParserM Tok
tokParser = lookAheadMatch block where
  block = MatchBlock chunk1 (fmap TokAtom atomTP)
    [ MatchCase Nothing commentPred commentTP
    , MatchCase Nothing (puncOrLineSepPred . T.head) puncOrLineSepTP
    ]

tokStreamParser :: TokParserM (Seq Tok)
tokStreamParser = greedyStarParser tokParser

parseTokStreamInteractive :: String -> IO (Maybe (Seq Tok))
parseTokStreamInteractive = parseInteractive tokStreamParser

parseTokStreamFileInteractive :: FilePath -> IO (Maybe (Seq Tok))
parseTokStreamFileInteractive = readFile >=> parseTokStreamInteractive

filterNonComments :: (x -> Tok) -> Seq x -> Seq x
filterNonComments p = Seq.filter ((\case { TokComment _ -> False; _ -> True }) . p)

dropLeadingNewlines :: (x -> Tok) -> Seq x -> Seq x
dropLeadingNewlines p = Seq.dropWhileL ((\case { TokNewline -> True; _ -> False }) . p)

dedupNewlines :: (x -> Tok) -> Seq x -> Seq x
dedupNewlines p = go Empty Nothing where
  go !acc !mx = \case
    Empty -> maybe acc (acc :|>) mx
    y :<| ys ->
      case mx of
        Nothing -> go acc (Just y) ys
        Just x ->
          if p y == TokNewline && p x == TokNewline
            then go acc mx ys
            else go (acc :|> x) (Just y) ys

xformStream :: LexedStream p Tok -> LexedStream p Tok
xformStream (LexedStream ss ep) = LexedStream (f ss) ep where
  p = spannedValue
  f = dedupNewlines p . dropLeadingNewlines p . filterNonComments p
