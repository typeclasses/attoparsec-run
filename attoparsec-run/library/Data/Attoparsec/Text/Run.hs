module Data.Attoparsec.Text.Run
  (
    parseStream, parseAndRestore, parseOnlyStrict, parseOnlyLazy,
    module Data.Attoparsec.Run,
  )
  where

import Data.Attoparsec.Run

import Control.Monad (unless)
import Control.Monad.State (evalState)
import Data.Attoparsec.Text (parseWith, Parser)
import Data.Text (Text, null)
import qualified Data.Text.Lazy as Lazy
import Prelude (($), (<$>), pure, mempty, Monad, Either)

parseOnlyStrict :: Text -> Parser a -> Either ParseError a
parseOnlyStrict x p = evalState (parseAndRestore inputState p) [x]

parseOnlyLazy :: Lazy.Text -> Parser a -> Either ParseError a
parseOnlyLazy x p = evalState (parseAndRestore inputState p) (Lazy.toChunks x)

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
