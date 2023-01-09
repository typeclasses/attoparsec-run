This package fixes a number of problems with the API that Attoparsec provides
for running parsers.

The difficulties stem from the that that Attoparsec's `IResult` type encompasses
three situations: When parsing has succeeded, when parsing has failed, and when
parsing is awaiting further input. This is insufficient to describe situations
in which we know we are dealing with a subset of these three cases. We address
this by introducing two smaller types: `FinalResult` and `ParseError`.

### FinalResult

`FinalResult` represents a result that we know not to be partial; for example,
the sort of result that we get when running a parser using `parseWith`.

```haskell
data FinalResult i a = FinalResult i (Either ParseError a)
```

### ParseError

`ParseError` represents only the case in which parsing has failed.

```haskell
data ParseError = ParseError [String] String
```

Our `showParseError` function gives an error string in the same format as
Attoparsec's `eitherResult` function.

```haskell
showParseError :: ParseError -> String
```

### BufferedInput

The `BufferedInput` type corresponds to two arguments of Attoparsec's
`parseWith`, the initial input and the action to obtain more input.

```haskell
data BufferedInput m i = BufferedInput i (m i)
```

In each of the modules `Data.Attoparsec.ByteString.Run` and
`Data.Attoparsec.Text.Run`, we provide a function called `parseStream`. This
closely corresponds to Attoparsec's `parseWith` function, but ours returns the
more specific `FinalResult` type, rather than `Result`, reflecting the fact that
a result returned here is never partial.

```haskell
parseStream :: Monad m =>
    BufferedInput m ByteString -> Parser a -> m (FinalResult ByteString a)
```

### RestorableInput

`RestorableInput` offers a new way to do streaming parsing that may be more
convenient than `parseStream` and `BufferedInput`. This type represents an input
stream with the ability to push unused input back to it.

```haskell
data RestorableInput m i = RestorableInput (m i) (i -> m ())
```

We use this type with the `parseAndRestore` function:

```haskell
parseAndRestore :: Monad m =>
    RestorableInput m ByteString -> Parser a -> m (Either ParseError a)
```

This shifts the burden of storing the unused remainder to feed back into the
next parsing step from the user of `parseStream` to the definer of the
`RestorableInput`.
