module Data.Attoparsec.Text.Run
  (
    parseStream, parseAndRestore,
    module Data.Attoparsec.Run,
  )
  where

import Data.Attoparsec.Run

import Control.Monad (unless)
import Data.Attoparsec.Text (parseWith, Parser)
import Data.Text (Text)
import Data.Text (null)
import Prelude (($), (<$>), pure, mempty, Monad, Either)

parseStream :: Monad m =>
    BufferedInput m Text -> Parser a -> m (FinalResult Text a)
parseStream (BufferedInput initial get) p =
    finalizeResult <$> parseWith get p initial

parseAndRestore :: Monad m =>
    RestorableInput m Text -> Parser a -> m (Either ParseError a)
parseAndRestore (RestorableInput get restore) p = do
    FinalResult remainder value <- parseStream (BufferedInput mempty get) p
    unless (null remainder) $ restore remainder
    pure value
