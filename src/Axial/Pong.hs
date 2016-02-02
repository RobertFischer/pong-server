module Axial.Pong
    (
      PongConfig(..)
    , defaultPongConfig
    , withPongServer
    ) where

data PongConfig = PongConfig {
  pongPort :: Int
} deriving (Eq,Ord,Read,Show)

defaultPongConfig :: PongConfig
defaultPongConfig = PongConfig 10411

withPongServer :: PongConfig -> IO a -> IO a
withPongServer cfg action = action
