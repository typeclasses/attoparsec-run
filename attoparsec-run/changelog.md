### 0.0.2.0 (2023-04-10)

New in `Data.Attoparsec.Run`:

```haskell
inputState :: (Monoid i, MonadState [i] m) => RestorableInput m i
```

New in `Data.Attoparsec.ByteString.run`:

```haskell
parseOnlyStrict :: ByteString -> Parser a -> Either ParseError a
parseOnlyLazy :: Lazy.ByteString -> Parser a -> Either ParseError a
```

New in `Data.Attoparsec.Text.run`:

```haskell
parseOnlyStrict :: Text -> Parser a -> Either ParseError a
parseOnlyLazy :: Lazy.Text -> Parser a -> Either ParseError a
```

### 0.0.1.0 (2023-01-09)

New function:

```haskell
newRestorableIO :: IO i -> IO (RestorableInput IO i)
```

This makes it easy to turn any `IO` input source into a `RestorableInput`,
backed by an `IORef` that holds any unparsed remainder.

### 0.0.0.0 (2023-01-09)

Initial release
