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
  describe "envConnectionInfo" $ do
    it "returns default settings in an empty environment" $ do
      let env = [("FAKTORY_PROVIDER", Nothing), ("FAKTORY_URL", Nothing)]

      withEnvironment env $ do
        connection <- envConnectionInfo
        connection `shouldBe` defaultConnectionInfo

    it "parses the provided URL" $ do
      let
        env =
          [("FAKTORY_PROVIDER", Nothing), ("FAKTORY_URL", Just "tcp://foo:123")]

      withEnvironment env $ do
        ConnectionInfo {..} <- envConnectionInfo
        connectionInfoTls `shouldBe` False
        connectionInfoPassword `shouldBe` Nothing
        connectionInfoHostName `shouldBe` "foo"
        connectionInfoPort `shouldBe` 123

    it "parses tls and password" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Nothing)
          , ("FAKTORY_URL", Just "tcp+tls://:foo@bar:123")
          ]

      withEnvironment env $ do
        ConnectionInfo {..} <- envConnectionInfo
        connectionInfoTls `shouldBe` True
        connectionInfoPassword `shouldBe` Just "foo"
        connectionInfoHostName `shouldBe` "bar"
        connectionInfoPort `shouldBe` 123

    it "follows _PROVIDER to find _URL" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Just "OTHER_URL")
          , ("OTHER_URL", Just "tcp://foo:123")
          ]

      withEnvironment env $ do
        ConnectionInfo {..} <- envConnectionInfo
        connectionInfoHostName `shouldBe` "foo"
        connectionInfoPort `shouldBe` 123

    it "throws nice errors for invalid PROVIDER" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Just "flippity-$flop")
          , ("FAKTORY_URL", Nothing)
          ]

      withEnvironment env envConnectionInfo
        `shouldThrowMessage` "expecting an environment variable name"

    it "throws nice errors for invalid _URL" $ do
      let
        env =
          [ ("FAKTORY_PROVIDER", Nothing)
          , ("FAKTORY_URL", Just "http://foo:123")
          ]

      withEnvironment env envConnectionInfo
        `shouldThrowMessage` "expecting tcp(+tls)://(:<password>@)<host>:<port>"

    it "throws nice errors for missing _PROVIDER" $ do
      pendingWith "This makes implementation more complicated"

      let
        env =
          [("FAKTORY_PROVIDER", Just "MISSING_URL"), ("MISSING_URL", Nothing)]

      withEnvironment env envConnectionInfo `shouldThrowMessage` "..."

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
shouldThrowMessage act msg =
  act `shouldThrow` \ex -> msg `isInfixOf` show @SomeException ex
