import Axial.Pong

import Data.Maybe (isJust, fromJust)

import Test.Hspec
import Test.QuickCheck
import Control.Exception (catch, SomeException)

import Network

import System.Timeout

import GHC.IO.Handle (Handle, hGetLine, hClose)


main :: IO ()
main = hspec $ do
  describe "Axial Pong server" $ do

    it "cannot be reached when not running" $ do
      reached <- serverReachable ()
      reached `shouldBe` False

    it "can be started and stopped twice" $ do
      reachedBefore <- serverReachable ()
      reachedBefore `shouldBe` False

      withPongServer defaultPongConfig $ do
        reached <- serverReachable ()
        reached `shouldBe` True

      reachedBetween <- serverReachable ()
      reachedBetween `shouldBe` False

      withPongServer defaultPongConfig $ do
        reached <- serverReachable ()
        reached `shouldBe` True

      reachedAfter <- serverReachable ()
      reachedAfter `shouldBe` False

    context "when running" $ around_ (withPongServer defaultPongConfig) $ do
      it "can be reached" $ do
        reached <- serverReachable ()
        reached `shouldBe` True

      it "can be reached twice" $ do
        reached1 <- serverReachable ()
        reached1 `shouldBe` True
        reached2 <- serverReachable ()
        reached2 `shouldBe` True

      it "survives after a client pre-emptively closes the connection" $ do
        handle <- connectServer ()
        hClose handle
        reached <- serverReachable ()
        reached `shouldBe` True

connectServer :: () -> IO Handle
connectServer () = withSocketsDo $ do
  connResult <- timeout (1000 * 1000) $ doConnect
  connResult `shouldSatisfy` isJust
  pure $ fromJust connResult
  where
    doConnect = connectTo serverName portNum
    portNum = PortNumber 10411
    serverName = "localhost"

serverReachable :: () -> IO Bool
serverReachable () = withSocketsDo $ catch tryConnect (\(e::SomeException) -> pure False)
  where
    tryConnect = do
      connResult <- timeout (1000 * 1000) doConnect
      pure $ isJust connResult
    doConnect = do
      socket <- connectTo serverName portNum
      receivedMessage <- hGetLine socket
      pure $ validateMessage receivedMessage
    portNum = PortNumber 10411
    serverName = "localhost"

validateMessage :: String -> Bool
validateMessage = (==) "pong"

