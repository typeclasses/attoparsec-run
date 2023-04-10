module Data.Attoparsec.Run where

import Data.Attoparsec.Types

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Prelude (Either (..), Eq, Ord, Show, String, IO, Monoid, mempty,
          pure, error, otherwise, null, ($), ($!), (++))
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State

data FinalResult i a = FinalResult
    i -- ^ Remaining unparsed input
    (Either ParseError a) -- ^ Either an error or a successfully parsed value
    deriving (Eq, Ord, Show)

finalizeResult ::
    IResult i a -- ^ Must be either 'Done' or 'Fail', not 'Partial'
    -> FinalResult i a
finalizeResult r = case r of
    Done remainder v ->
        FinalResult remainder (Right v)
    Fail remainder context message ->
        FinalResult remainder (Left (ParseError context message))
    Partial{} ->
        error "parseWith should not return Partial"

data ParseError = ParseError
    [String] -- ^ A list of contexts in which the error occurred
    String -- ^ The message describing the error, if any
    deriving (Eq, Ord, Show)

-- | Format a parse error in a matter suitable for displaying in log output
showParseError :: ParseError -> String
showParseError (ParseError context message)
    | null context = message
    | otherwise = intercalate " > " context ++ ": " ++ message

data BufferedInput m i = BufferedInput
    i -- ^ Initial input
    (m i) -- ^ Get the next chunk of input, or an empty string if the
          --   end of input has been reached

{-| An effectful source of parser input which supports a "restore" operation
    that can be used to push unused portions of input back to the source

For an example, see 'newRestorableIO'. -}
data RestorableInput m i = RestorableInput
    (m i) -- ^ Get the next chunk of input, or an empty string if the
          --   end of input has been reached
    (i -> m ()) -- ^ Restore a non-empty chunk of input to the input stream

{-| Turn any 'IO' input source into a 'RestorableInput'

Internally, this is backed by an 'IORef' that holds any unparsed remainder. -}
newRestorableIO :: IO i -> IO (RestorableInput IO i)
newRestorableIO unbufferedGet = do
    buffer <- newIORef [] -- The buffer stores the unparsed inputs
                          -- that have pushed back by "restore".
    pure $ RestorableInput (get buffer) (restore buffer)

  where
    -- Restoring writes an input chunk to the top of the stack.
    restore buffer x = do
        xs <- readIORef buffer
        writeIORef buffer $! (x : xs)

    get buffer = do
        bufferContent <- readIORef buffer
        case bufferContent of

            -- If the buffer is empty, then "get" just runs the action.
            [] -> unbufferedGet

            -- If there is content that has been pushed back onto the
            -- buffer, then "get" pops a chunk off of the stack instead
            -- of running the action.
            (x : xs) -> do
                writeIORef buffer $! xs
                pure x

{-| A 'RestorableInput' in which getting and restoring are both backed
    by 'MonadState' operations. -}
inputState :: (Monoid i, MonadState [i] m) => RestorableInput m i
inputState = RestorableInput get restore
  where
    get = do
        xs <- State.get
        case xs of
            [] -> pure mempty
            (x : xs') -> do
                State.put xs'
                pure x

    restore x = State.modify' (x :)
