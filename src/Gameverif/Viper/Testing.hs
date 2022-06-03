{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Only for testing
module Gameverif.Viper.Testing where

import Data.String.QQ (s)
import Data.Text (Text)
import Gameverif.Common.Decode (decodeConsole, renderDecodeResult)
import Gameverif.Util.Console (withConsoleM)
import Gameverif.Viper.Concrete (forgetAnnProg)
import Gameverif.Viper.Parser (VipProg, vipProgParser)
import Gameverif.Viper.Printer (printProg)
import Gameverif.Viper.Process (invokeViper)

loadProg :: FilePath -> IO (VipProg Text)
loadProg fp = withConsoleM fp (decodeConsole vipProgParser >>= renderDecodeResult)

loadTestProg :: IO (VipProg Text)
loadTestProg = loadProg "testdata/test.vpr"

sanityProg :: IO ()
sanityProg = loadTestProg >>= print . printProg . forgetAnnProg

vipProgOk :: Text
vipProgOk = [s|
method foo(i: Int)
  requires i > 0
{
  assert i > 0;
}
|]

vipProgFail :: Text
vipProgFail = [s|
method foo(i: Int)
{
  assert i > 0;
}

method bar(i: Int)
{
  assert i < 0;
}
|]

tryVipProgOk :: IO ()
tryVipProgOk = do
  code <- invokeViper "xok.vpr" vipProgOk
  print code

tryVipProgFail :: IO ()
tryVipProgFail = do
  code <- invokeViper "xfail.vpr" vipProgFail
  print code
