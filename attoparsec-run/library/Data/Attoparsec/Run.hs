module Data.Attoparsec.Run where

import Data.Attoparsec.Types

import Data.List (intercalate)
import Prelude (Either (..), Eq, Ord, Show, String, error, otherwise, null, (++))

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
    (m i) -- ^ Should return an empty string once the end of input is reached

data RestorableInput m i = RestorableInput
    (m i) -- ^ Return an empty string once the end of input is reached
    (i -> m ()) -- ^ Return a non-empty chunk of input to the input stream
