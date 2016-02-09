module Axial.Pong
    (
      PongConfig(..)
    , defaultPongConfig
    , withPongServer
    ) where

import Network (withSocketsDo, listenOn, accept, sClose, PortID(..))
import System.IO (hClose, hPutStr, hSetBuffering, BufferMode(..))
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO, forkFinally, ThreadId, killThread)

data PongConfig = PongConfig {
  pongPort :: Int,
  pongMessage :: IO String
}

newtype PongServer = PongServer ThreadId

defaultPongConfig :: PongConfig
defaultPongConfig = PongConfig 10411 $ pure "pong"

withPongServer :: PongConfig -> IO a -> IO a
withPongServer cfg action = bracket (startServer cfg) stopServer $ const action

startServer :: PongConfig -> IO PongServer
startServer cfg = withSocketsDo $ do
  socket <- listenOn portNum
  threadId <- forkIO $ socketHandler socket
  pure $ PongServer threadId
  where
    socketHandler sock = finally (socketHandlerLoop sock) (sClose sock)
    socketHandlerLoop sock = do
      (handle, _, _) <- accept sock
      _ <- forkFinally (body handle) (const $ hClose handle)
      socketHandlerLoop sock
    body handle = do
      msg <- (pongMessage cfg)
      hSetBuffering handle NoBuffering
      hPutStr handle msg
    portNum = PortNumber $ fromIntegral pongPortNum
    pongPortNum = pongPort cfg

stopServer :: PongServer -> IO ()
stopServer (PongServer threadId) = killThread threadId
