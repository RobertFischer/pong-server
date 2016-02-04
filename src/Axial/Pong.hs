module Axial.Pong
    (
      PongConfig(..)
    , PongServer
    , defaultPongConfig
    , withPongServer
    ) where

import Network
import System.IO (hClose, hFlush, hPutStr, hSetBuffering, BufferMode(..), Handle)
import Control.Exception (bracket)
import Control.Concurrent (forkIO)

data PongConfig = PongConfig {
  pongPort :: Int,
  pongMessage :: () -> IO String
}

newtype PongServer = PongServer Socket

defaultPongConfig :: PongConfig
defaultPongConfig = PongConfig 10411 $ const $ pure "pong"

withPongServer :: PongConfig -> IO a -> IO a
withPongServer cfg action = bracket (startServer cfg) stopServer $ const action

startServer :: PongConfig -> IO PongServer
startServer cfg = withSocketsDo $ do
  sock <- listenOn portNum
  forkIO $ socketHandler sock
  pure $ PongServer sock
  where
    socketHandler sock = do
      (handle, _, _) <- accept sock
      forkIO $ body handle
      socketHandler sock
    body handle = do
      msg <- (pongMessage cfg) ()
      hSetBuffering handle NoBuffering
      hPutStr handle msg
      hFlush handle
      hClose handle
    portNum = PortNumber pongPortNum
    pongPortNum = pongPort cfg

stopServer :: PongServer -> IO ()
stopServer (PongServer sock) = sClose sock
