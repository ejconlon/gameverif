{-# LANGUAGE OverloadedStrings #-}

-- | Only for testing
module Gameverif.Ecsy.Testing where

import Data.Text (Text)
import Gameverif.Common.Decode (decodeConsole, renderDecodeResult)
import Gameverif.Ecsy.Concrete (forgetAnnProg)
import Gameverif.Ecsy.Parser (EcsyProg, ecsyProgParser)
import Gameverif.Ecsy.Printer (printProg)
import Gameverif.Util.Console (withConsoleM)

loadProg :: FilePath -> IO (EcsyProg Text)
loadProg fp = withConsoleM fp (decodeConsole ecsyProgParser >>= renderDecodeResult)

loadTestProg :: IO (EcsyProg Text)
loadTestProg = loadProg "testdata/test.ecsy"

sanityProg :: IO ()
sanityProg = loadTestProg >>= print . printProg . forgetAnnProg
