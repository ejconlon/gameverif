module Gameverif.Common.Decode where

import Control.Monad ((>=>))
import Control.Monad.Reader (ask)
import Data.Foldable (toList)
import Data.Text (Text)
import Errata (Block, PointerStyle, Style)
import Gameverif.Common.Lexer (tokParser, xformStream)
import Gameverif.Common.Parser (ParserM)
import Gameverif.Util.Console (ConsoleEnv (..), ConsoleM (..), renderBlocksM)
import SimpleParser (ParseErrorBundle (..), ParseResult (..), ParseSuccess (..), lexedParseInteractive, lexedParser,
                     matchEnd, newLinePosStream, runParser)
import SimpleParser.Errata (errataParseError)

decodeInteractive :: ParserM a -> String -> IO (Maybe a)
decodeInteractive = lexedParseInteractive tokParser xformStream

decodeFileInteractive :: ParserM a -> FilePath -> IO (Maybe a)
decodeFileInteractive p = readFile >=> decodeInteractive p

data DecodeResult a =
    DecodeResLexErr ![Block]
  | DecodeResLexEmpty
  | DecodeResParseErr ![Block]
  | DecodeResParseEmpty
  | DecodeResParseSuccess !a
  deriving stock (Show, Functor)

decodeErrata :: ParserM a -> Style -> PointerStyle -> FilePath -> Text -> DecodeResult a
decodeErrata p bsty psty fp contents =
  let mkBlocksLex = fmap (errataParseError bsty psty fp) . toList
      mkBlocksParse = fmap (errataParseError bsty psty fp) . toList
      lexRes = runParser (lexedParser tokParser <* matchEnd) (newLinePosStream contents)
  in case lexRes of
    Just (ParseResultSuccess (ParseSuccess _ ls)) ->
      let parseRes = runParser (p <* matchEnd) (xformStream ls)
      in case parseRes of
        Just (ParseResultSuccess (ParseSuccess _ a)) -> DecodeResParseSuccess a
        Just (ParseResultError (ParseErrorBundle errs)) -> DecodeResParseErr (mkBlocksLex errs)
        Nothing -> DecodeResParseEmpty
    Just (ParseResultError (ParseErrorBundle errs)) -> DecodeResLexErr (mkBlocksParse errs)
    Nothing -> DecodeResLexEmpty

decodeConsole :: ParserM a -> ConsoleM (DecodeResult a)
decodeConsole p = do
  ConsoleEnv bsty psty fp contents <- ask
  pure (decodeErrata p bsty psty fp contents)

renderDecodeResult :: DecodeResult a -> ConsoleM a
renderDecodeResult = \case
  DecodeResLexErr bls -> renderBlocksM bls *> fail "lex error"
  DecodeResLexEmpty -> fail "lex empty"
  DecodeResParseErr bls -> renderBlocksM bls *> fail "parse error"
  DecodeResParseEmpty -> fail "parse empty"
  DecodeResParseSuccess a -> pure a
