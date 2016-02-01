module Axial.Pong
    (
      PongConfig { ... }
    , defaultPongConfig
    ) where

data PongConfig {
  pongPort :: Int
}

defaultPongConfig = PongConfig 10411

withPongServer :: IO a -> IO a
withPongServer = id
