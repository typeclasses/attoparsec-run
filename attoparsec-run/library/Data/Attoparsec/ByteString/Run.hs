module Data.Attoparsec.ByteString.Run
  (
    parseStream, parseAndRestore,
    module Data.Attoparsec.Run,
  )
  where

import Data.Attoparsec.Run

import Control.Monad (unless)
import Data.Attoparsec.ByteString (parseWith, Parser)
import Data.ByteString (ByteString)
import Data.ByteString (null)
import Prelude (($), (<$>), pure, mempty, Monad, Either)

parseStream :: Monad m =>
    BufferedInput m ByteString -> Parser a -> m (FinalResult ByteString a)
parseStream (BufferedInput initial get) p =
    finalizeResult <$> parseWith get p initial

parseAndRestore :: Monad m =>
    RestorableInput m ByteString -> Parser a -> m (Either ParseError a)
parseAndRestore (RestorableInput get restore) p = do
    FinalResult remainder value <- parseStream (BufferedInput mempty get) p
    unless (null remainder) $ restore remainder
    pure value
