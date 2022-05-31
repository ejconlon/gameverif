-- | Only for testing
module Gameverif.Viper.Testing where

import Data.Text (Text)
import Gameverif.Util.Console (withConsoleM)
import Gameverif.Viper.Concrete (forgetAnnProg)
import Gameverif.Viper.Parser (parseVipProgramConsole, renderVipParseResult)
import Gameverif.Viper.Plain (PlainProg)

loadProg :: FilePath -> IO (PlainProg Text)
loadProg fp = do
  p <- withConsoleM fp (parseVipProgramConsole >>= renderVipParseResult)
  pure (forgetAnnProg p)

loadTestProg :: IO (PlainProg Text)
loadTestProg = loadProg "testdata/test.vpr"
