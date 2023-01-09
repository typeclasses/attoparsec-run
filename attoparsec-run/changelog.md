### 0.0.1.0 (2023-01-09)

New function:

```haskell
newRestorableIO :: IO i -> IO (RestorableInput IO i)
```

This makes it easy to turn any `IO` input source into a `RestorableInput`,
backed by an `IORef` that holds any unparsed remainder.

### 0.0.0.0 (2023-01-09)

Initial release
