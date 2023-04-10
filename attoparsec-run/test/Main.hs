module Main (main) where

import Test.Hspec

import Control.Applicative (liftA2)
import Data.Attoparsec.Combinator ((<?>))
import Data.Attoparsec.Run (ParseError (ParseError))
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Attoparsec.ByteString as P.BS
import qualified Data.Attoparsec.ByteString.Run as Run.BS
import qualified Data.Attoparsec.Text as P.Text
import qualified Data.Attoparsec.Text.Run as Run.Text

main :: IO ()
main = hspec $ do

    describe "ByteString" $ do

        describe "parseOnlyStrict" $ do

            specify "success" $
                Run.BS.parseOnlyStrict "abc" (P.BS.anyWord8 <?> "1")
                    `shouldBe` Right 97

            specify "failure" $
                Run.BS.parseOnlyStrict ("abc") (P.BS.satisfy (< 97) <?> "1")
                    `shouldBe` Left (ParseError ["1"] "Failed reading: satisfy")

        describe "parseOnlyLazy" $ do

            specify "two chunks" $
                Run.BS.parseOnlyLazy
                    (BS.Lazy.fromChunks ["a", "b"])
                    (liftA2 (,) P.BS.anyWord8 P.BS.anyWord8)
                    `shouldBe` Right (97, 98)

    describe "Text" $ do

        describe "parseOnlyStrict" $ do

            specify "success" $
                Run.Text.parseOnlyStrict "abc" P.Text.anyChar
                    `shouldBe` Right 'a'

            specify "failure" $
                Run.Text.parseOnlyStrict ("abc") (P.Text.satisfy (< 'a') <?> "1")
                    `shouldBe` Left (ParseError ["1"] "Failed reading: satisfy")

        describe "parseOnlyLazy" $ do

            specify "two chunks" $
                Run.Text.parseOnlyLazy
                    (Text.Lazy.fromChunks ["a", "b"])
                    (liftA2 (,) P.Text.anyChar P.Text.anyChar)
                    `shouldBe` Right ('a', 'b')
