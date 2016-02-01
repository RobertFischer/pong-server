import Axial.Pong

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, bracket)

import Network


main :: IO ()
main = hspec $ do
  describe "Axial Pong server" $ do

    it "cannot be reached when not running" $ do
      reached <- serverReachable ()
      reached `shouldBe` False

    context "when running" $ around_ withPongServer $ do
      it "can be reached" $ do
        reached <- serverReachable ()
        reached `shouldBe` True

serverReachable :: () -> IO Boolean
serverReachable () = withSocketsDo $ do
  receivedMessage <- recieveFrom "localhost" (pongPort defaultPongConfig)
  pure (validateMessage receivedMessage)

validateMesage :: String -> Boolean
validateMesage = (==) "pong"
