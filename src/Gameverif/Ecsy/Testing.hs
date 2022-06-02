{-# LANGUAGE OverloadedStrings #-}

-- | Only for testing
module Gameverif.Ecsy.Testing where

import Data.Text (Text)
import Gameverif.Common.Decode (decodeConsole, renderDecodeResult)
import Gameverif.Ecsy.Concrete (forgetAnnProg)
import Gameverif.Ecsy.Parser (ecsyProgParser)
import Gameverif.Ecsy.Plain (PlainProg)
import Gameverif.Util.Console (withConsoleM)

loadProg :: FilePath -> IO (PlainProg Text)
loadProg fp = do
  p <- withConsoleM fp (decodeConsole ecsyProgParser >>= renderDecodeResult)
  pure (forgetAnnProg p)

loadTestProg :: IO (PlainProg Text)
loadTestProg = loadProg "testdata/test.ecsy"
