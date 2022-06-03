{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Common.Parser where

import Control.Monad.State.Strict (gets, void)
import Data.Text (Text)
import Data.Void (Void)
import Gameverif.Common.Lexer (Atom (..), Tok (..))
import SimpleParser (ExplainLabel (..), LexedSpan (..), LexedStream, LinePos, Parser, Span (..), anyToken,
                     betweenParser, dropTokensWhile, lexemeParser, markParser, satisfyToken, streamViewPos)
import qualified Text.Builder as TB

data Label =
    LabelKeyword !Text
  | LabelNewline
  | LabelIdent
  | LabelBuiltOp
  | LabelInt
  | LabelComma
  | LabelPeriod
  | LabelQuestion
  | LabelColon
  | LabelDoubleColon
  | LabelColonEquals
  | LabelOpenParen
  | LabelCloseParen
  | LabelOpenBrace
  | LabelCloseBrace
  | LabelOpenBracket
  | LabelCloseBracket
  deriving stock (Eq, Show)

instance ExplainLabel Label where
  explainLabel = \case
    LabelKeyword t -> "keyword " <> TB.text t
    LabelNewline -> "newline/semicolon"
    LabelIdent -> "identifier"
    LabelBuiltOp -> "built-in op"
    LabelInt -> "integer"
    LabelComma -> "comma"
    LabelPeriod -> "period"
    LabelQuestion -> "question"
    LabelColon -> "colon"
    LabelDoubleColon -> "double colon"
    LabelColonEquals -> "colon equals"
    LabelOpenParen -> "open paren"
    LabelCloseParen -> "close paren"
    LabelOpenBrace -> "open brace"
    LabelCloseBrace -> "close brace"
    LabelOpenBracket -> "open bracket"
    LabelCloseBracket -> "close bracket"

type Extent = Span LinePos
type ParserM a = Parser Label (LexedStream LinePos Tok) Void a

startLexed :: LexedSpan p -> p
startLexed = \case
  LexedSpanElem s -> spanStart s
  LexedSpanEnd p -> p

spanLexed :: LexedSpan LinePos -> LexedSpan LinePos -> Extent
spanLexed ls0 ls1 = Span (startLexed ls0) (startLexed ls1)

withPos :: (Extent -> a -> b) -> ParserM a -> ParserM b
withPos f m = do
  ls0 <- gets streamViewPos
  a <- m
  ls1 <- gets streamViewPos
  let s = spanLexed ls0 ls1
      b = f s a
  pure b

tokSpacePred, newlinePred, commaPred, periodPred, questionPred, colonPred, colonEqualsPred, doubleColonPred, openParenPred,
  closeParenPred, openBracePred, closeBracePred, openBracketPred, closeBracketPred :: Tok -> Bool
tokSpacePred = \case
  TokNewline -> True
  TokComment _ -> True
  _ -> False
newlinePred = \case { TokNewline -> True; _ -> False }
commaPred = \case { TokComma -> True; _ -> False }
questionPred = \case { TokQuestion -> True; _ -> False }
colonPred = \case { TokColon -> True; _ -> False }
periodPred = \case { TokPeriod -> True; _ -> False }
doubleColonPred = \case { TokDoubleColon -> True; _ -> False }
colonEqualsPred = \case { TokColonEquals -> True; _ -> False }
openParenPred = \case { TokOpenParen -> True; _ -> False }
closeParenPred = \case { TokCloseParen -> True; _ -> False }
openBracePred = \case { TokOpenBrace -> True; _ -> False }
closeBracePred = \case { TokCloseBrace -> True; _ -> False }
openBracketPred = \case { TokOpenBracket -> True; _ -> False }
closeBracketPred = \case { TokCloseBracket -> True; _ -> False }

keywordPred :: Text -> Tok -> Bool
keywordPred t = \case
  TokAtom (AtomIdent s) | s == t -> True
  _ -> False

spaceP, newlineP :: ParserM ()
spaceP = void (dropTokensWhile tokSpacePred)
newlineP = void (satisfyToken (Just LabelNewline) newlinePred)

lexP :: ParserM a -> ParserM a
lexP = lexemeParser spaceP

rawCommaP, rawPeriodP, rawQuestionP, rawColonP, rawDoubleColonP, rawColonEqualsP, rawOpenParenP, rawCloseParenP, rawOpenBraceP, rawCloseBraceP, rawOpenBracketP, rawCloseBracketP :: ParserM ()
rawCommaP = void (satisfyToken (Just LabelComma) commaPred)
rawPeriodP = void (satisfyToken (Just LabelPeriod) periodPred)
rawQuestionP = void (satisfyToken (Just LabelQuestion) questionPred)
rawColonP = void (satisfyToken (Just LabelColon) colonPred)
rawDoubleColonP = void (satisfyToken (Just LabelDoubleColon) doubleColonPred)
rawColonEqualsP = void (satisfyToken (Just LabelColonEquals) colonEqualsPred)
rawOpenParenP = void (satisfyToken (Just LabelOpenParen) openParenPred)
rawCloseParenP = void (satisfyToken (Just LabelCloseParen) closeParenPred)
rawOpenBraceP = void (satisfyToken (Just LabelOpenBrace) openBracePred)
rawCloseBraceP = void (satisfyToken (Just LabelCloseBrace) closeBracePred)
rawOpenBracketP = void (satisfyToken (Just LabelOpenBracket) openBracketPred)
rawCloseBracketP = void (satisfyToken (Just LabelCloseBracket) closeBracketPred)

commaP, periodP, questionP, colonP, doubleColonP, colonEqualsP, openParenP, closeParenP, openBraceP, closeBraceP, openBracketP, closeBracketP :: ParserM ()
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

betweenParensP, betweenBracesP, betweenBracketsP :: ParserM a -> ParserM a
betweenParensP = betweenParser openParenP closeParenP
betweenBracesP = betweenParser openBraceP closeBraceP
betweenBracketsP = betweenParser openBracketP closeBracketP

rawIdentP, identP :: ParserM Text
rawIdentP = markParser (Just LabelIdent) $ do
  tok <- anyToken
  case tok of
    TokAtom (AtomIdent s) -> pure s
    _ -> fail "expected identifier"
identP = lexP rawIdentP

rawIntP, intP :: ParserM Integer
rawIntP = markParser (Just LabelInt) $ do
  tok <- anyToken
  case tok of
    TokAtom (AtomInt i) -> pure i
    _ -> fail "expected integer"
intP = lexP rawIntP

rawKeywordP, keywordP :: Text -> ParserM ()
rawKeywordP t = void (satisfyToken (Just (LabelKeyword t)) (keywordPred t))
keywordP = lexP . rawKeywordP
