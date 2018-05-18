module Faktory.ConnectionSpec
  ( spec
  ) where

import Faktory.Prelude

import Data.List (isInfixOf)
import Faktory.Connection
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
  describe "envConnection" $ do
    it "returns default settings in an empty environment" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Nothing)
          , ("FAKTORY_URL", Nothing)
          ]

      withEnvironment env $ do
        connection <- envConnection
        connection `shouldBe` defaultConnection

    it "parses the provided URL" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Nothing)
          , ("FAKTORY_URL", Just "tcp://foo:123")
          ]

      withEnvironment env $ do
        Connection{..} <- envConnection
        connectionHostName `shouldBe` "foo"
        connectionPort `shouldBe` 123

    it "follows _PROVIDER to find _URL" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Just "OTHER_URL")
          , ("OTHER_URL", Just "tcp://foo:123")
          ]

      withEnvironment env $ do
        Connection{..} <- envConnection
        connectionHostName `shouldBe` "foo"
        connectionPort `shouldBe` 123

    it "throws nice errors for invalid PROVIDER" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Just "flippity-$flop")
          , ("FAKTORY_URL", Nothing)
          ]

      withEnvironment env envConnection
        `shouldThrowMessage` "expecting an environment variable name"

    it "throws nice errors for invalid _URL" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Nothing)
          , ("FAKTORY_URL", Just "http://foo:123")
          ]

      withEnvironment env envConnection
        `shouldThrowMessage` "expecting tcp(+tls)://(:<password>@)<host>:<port>"

    it "throws nice errors for missing _PROVIDER" $ do
      pendingWith "This makes implementation more complicated"

      let
        env =
          [ ("FAKTORY_PROVIDER", Just "MISSING_URL")
          , ("MISSING_URL", Nothing)
          ]

      withEnvironment env envConnection `shouldThrowMessage` "..."

-- | Override ENV with the given values, and restore them after
--
-- Values are @'Maybe'@ so that @'Nothing'@ can be used to unset values during
-- override and/or restoration. Note: this is probably not thread-safe.
--
withEnvironment :: [(String, Maybe String)] -> IO a -> IO a
withEnvironment env act = bracket
  (traverse readAndReset env)
  (traverse_ readAndReset)
  (const act)
 where
  readAndReset :: (String, Maybe String) -> IO (String, Maybe String)
  readAndReset (variable, mNewValue) = do
    mOriginalValue <- lookupEnv variable
    maybe (unsetEnv variable) (setEnv variable) mNewValue
    pure (variable, mOriginalValue)

shouldThrowMessage :: IO a -> String -> Expectation
shouldThrowMessage act msg = act `shouldThrow` \ex ->
  msg `isInfixOf` show @SomeException ex
