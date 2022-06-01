module Gameverif.Util.Console where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Errata (Block, Errata (..), PointerStyle, Style, blockMerged', prettyErrors)
import Errata.Styles (fancyPointer, fancyStyle)
import SimpleParser (Col (Col), Line (Line), LinePos (LinePos), Span (Span))

defaultBsty :: Style
defaultBsty = fancyStyle

defaultPsty :: PointerStyle
defaultPsty = fancyPointer

renderErrata :: Text -> [Block] -> IO ()
renderErrata contents blocks = do
  let errata = Errata Nothing blocks Nothing
      pretty = prettyErrors contents [errata]
  TLIO.putStr pretty

data ConsoleEnv = ConsoleEnv
  { ceBsty :: !Style
  , cePsty :: !PointerStyle
  , ceFilePath :: !FilePath
  , ceContents :: !Text
  }

defaultConsoleEnv :: FilePath -> Text -> ConsoleEnv
defaultConsoleEnv = ConsoleEnv defaultBsty defaultPsty

newConsoleEnv :: FilePath -> IO ConsoleEnv
newConsoleEnv fp = do
  contents <- TIO.readFile fp
  pure (defaultConsoleEnv fp contents)

interactiveFilePath :: FilePath
interactiveFilePath = "<interactive>"

interactiveConsoleEnv :: Text -> ConsoleEnv
interactiveConsoleEnv = defaultConsoleEnv interactiveFilePath

renderBlocksM :: [Block] -> ConsoleM ()
renderBlocksM blocks = do
  contents <- asks ceContents
  liftIO (renderErrata contents blocks)

simpleBlockM :: Span LinePos -> Text -> ConsoleM Block
simpleBlockM (Span (LinePos _ (Line l0) (Col c0)) (LinePos _ (Line l1) (Col c1))) msg = do
  ConsoleEnv bsty psty fp _ <- ask
  let start = (l0 + 1, c0 + 1, Nothing)
      end = (l1 + 1, c1 + 1, Nothing)
  pure (blockMerged' bsty psty fp Nothing start end Nothing (Just msg))

newtype ConsoleM a = ConsoleM { unConsoleM :: ReaderT ConsoleEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader ConsoleEnv, MonadIO, MonadUnliftIO, MonadFail, MonadThrow)

runConsoleM :: ConsoleM a -> ConsoleEnv -> IO a
runConsoleM m = runReaderT (unConsoleM m)

withConsoleM :: FilePath -> ConsoleM a -> IO a
withConsoleM fp act = do
  env <- newConsoleEnv fp
  runConsoleM act env

interactiveConsoleM :: Text -> ConsoleM a -> IO a
interactiveConsoleM contents act =
  let env = interactiveConsoleEnv contents
  in runConsoleM act env
