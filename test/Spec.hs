import Axial.Pong

import Data.Maybe (isJust)

import Test.Hspec
import Test.QuickCheck
import Control.Exception (catch, SomeException)

import Network

import System.Timeout

import GHC.IO.Handle (hGetLine)


main :: IO ()
main = hspec $ do
  describe "Axial Pong server" $ do

    it "cannot be reached when not running" $ do
      reached <- serverReachable ()
      reached `shouldBe` False

    context "when running" $ around_ (withPongServer defaultPongConfig) $ do
      it "can be reached" $ do
        reached <- serverReachable ()
        reached `shouldBe` True

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

