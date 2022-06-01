{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Viper.Process where

import qualified Data.ByteString.Lazy as BSL
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import SimpleParser (Span (..), Col (..), Line (..), Parser, TextLabel, TextualStream, decimalParser, LazyCharString (..))
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import qualified System.Process.Typed as Q
import Text.Scanf (Format, (:+) (..), fmt, scanf)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char (isSpace)
import qualified Data.Text.Encoding as TE
import Data.Void (Void)
import qualified SimpleParser as SP

data LineCol = LineCol !Line !Col
  deriving stock (Eq, Show)

data Failure = Failure !Text !(Span LineCol)
  deriving stock (Eq, Show)

data Result =
    ResultOk
  | ResultFail ![Failure]
  deriving stock (Eq, Show)

failureParser :: Parser TextLabel LazyCharString Void Failure
failureParser = do
  _ <- SP.dropTokensWhile isSpace
  _ <- SP.matchToken '['
  _ <- SP.decimalParser
  _ <- SP.matchToken ']'
  _ <- SP.dropTokensWhile1 Nothing isSpace
  msg <- SP.takeTokensWhile1 Nothing (/= '(')
  _ <- SP.matchToken '('
  _ <- SP.dropTokensWhile1 Nothing (/= '@')
  _ <- SP.matchToken '@'
  startLine <- SP.decimalParser
  _ <- SP.matchToken '.'
  startCol <- SP.decimalParser
  _ <- SP.matchChunk "--"
  endLine <- SP.decimalParser
  _ <- SP.matchToken '.'
  endCol <- decimalParser
  _ <- SP.matchToken ')'
  let start = LineCol (Line startLine) (Col startCol)
      end = LineCol (Line endLine) (Col endCol)
  pure $! Failure (TE.decodeUtf8 (BSL.toStrict (unLazyCharString msg))) (Span start end)

parseFailure :: BSL.ByteString -> Maybe Failure
parseFailure = fmap SP.psValue . SP.runParserThrow failureParser . LazyCharString

invokeViper :: FilePath -> Text -> IO Result
invokeViper name contents = do
  vipHome <- getEnv "VIPER_HOME"
  let vipBin = vipHome </> "bin" </> "viper"
  withSystemTempFile name $ \target handle -> do
    hClose handle
    TIO.writeFile target contents
    (code, interleaved) <- Q.readProcessInterleaved (Q.proc vipBin ["carbon", target])
    BSL.putStr interleaved
    print code
    pure $! case code of
      ExitSuccess -> ResultOk
      ExitFailure _ ->
        let xs = BSLC.lines interleaved
        in ResultFail (xs >>= maybe [] pure . parseFailure)
