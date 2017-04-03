{-|
Module      : Network.Pong
Description : Ping server that runs in the background.
Copyright   : (c) Robert Fischer, 2017
License     : Unlicense
Maintainer  : smokejumper+network.pong@gmail.com
Stability   : experimental
Portability : POSIX


This is an exceedingly simple server: it listens on the given port on all interfaces,
and when it retrieves a connection, it simply prints out the result of the 'pongMessage'
action, which is just the four characters @pong@ by default.

The purpose of this server is to provide something for automated smoke checks (such as
load balancers) to hit against. It provides a rudimentary server smoke check while
requiring a minimal amount of overhead.

There are two customary ways to use this server. The first customary way uses whether
the response is empty or not to signal whether there is an error or not. If the resulting
body is empty, there is an error. If the response is non-empty, the server is fine. This
is supported by 'pongCatch'. The
second customary way to use this server is to treat it as a server running @HTTP/0.9@,
and to use 200 response status codes to mean the server is good and 500 response status codes
to mean the server is having a problem. This is supported by the 'pongCatchHttp' and the other
@pong*Http*@ functions.

A third way to use this server is to have the body of the message actually contain some
meaningful values about the state of the system. The reason the response is a 'ByteString'
is specifically so that you could implement a binary protocol if you deeply wanted to.
If you are heading down that path, though, please first look into the @ekg@ package. Keep
'Network.Pong' in play for automated smoke checks, and use @ekg@ for more precise status
monitoring.

To run this server, simply add this into your main function:

@
  main = withPongServer defaultPongConfig $ do
@

It will then run the pong server on port @10411@. Of course, you can customize the
server through the configuration object.

-}
module Network.Pong
    (
      PongConfig(..)
    , defaultPongConfig
    , pongMessageHttp200
    , pongMessageHttp500
    , pongMessageHttp
    , pongCatch
    , pongCatchHttp
    , withPongServer
    ) where

import Prelude ()
import qualified Data.ByteString.Char8 as C
import ClassyPrelude hiding (catch, finally)
import Control.Concurrent (forkIO)
import Control.Monad.Catch (catch, finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Types.Status (ok200, internalServerError500, Status(..))
import Network (withSocketsDo, listenOn, accept, sClose, PortID(..), Socket)
import System.IO (hClose, hSetBuffering, BufferMode(..))

-- | This provides the configuration for the pong server.
data PongConfig m = PongConfig {
  pongPort :: Int, -- ^ The port that this server will run on
  pongMessage :: m ByteString -- ^ The action which generates a response message
}

pongMessageHttp200 :: (Monad m) => m ByteString
-- ^ Provides an action generating an @HTTP/0.9@ response message for the 'ok200' 'Status'.
pongMessageHttp200 = pongMessageHttp ok200

pongMessageHttp500 :: (Monad m) => m ByteString
-- ^ Provides an action generating an @HTTP/0.9@ response message for the 'internalServerError500' 'Status'.
pongMessageHttp500 = pongMessageHttp internalServerError500

pongMessageHttp :: (Monad m) => Status -> m ByteString
-- ^ Provides an action generating an @HTTP/0.9@ response message for the given 'Status'.
pongMessageHttp status =
  return $ concat ["HTTP/0.9 ", statusCodeBS status, " ", statusMessage status]
    where
      statusCodeBS :: Status -> ByteString
      statusCodeBS = C.pack . show . statusCode

pongCatchHttp :: (MonadCatch m, MonadIO m, Exception e) => m Status -> (e -> m Status) -> m ByteString
-- ^ Allows for customization of the result 'Status' given different exceptions.
pongCatchHttp good bad = pongCatch goodBS badBS
  where
    goodBS = do
      goodValue <- good
      liftIO $ pongMessageHttp goodValue

    badBS = \e -> do
      badValue <- bad e
      liftIO $ pongMessageHttp badValue

pongCatch :: (MonadCatch m, MonadIO m, Exception e) => m ByteString -> (e -> m ByteString) -> m ByteString
-- ^ Allows for customization of the result message given different exceptions.
pongCatch = catch -- Yes, we could have just let people use 'catch', but it may be non-obvious

-- | Handle for a running server
newtype PongServer = PongServer ThreadId

defaultPongConfig :: (Monad m) => PongConfig m
-- ^ Default config that runs on port @10411@ and just prints out the four characters @pong@.
defaultPongConfig = PongConfig 10411 $ (return $ C.pack $ "pong")

withPongServer :: (MonadIO m, MonadMask m) => PongConfig m -> m a -> m a
-- ^ Entry point to the pong server.
withPongServer cfg action = bracket (startServer cfg) stopServer $ const action

startServer :: (MonadIO m) => PongConfig m -> m PongServer
-- ^ Implementation of actually starting the server.
startServer cfg = liftIO withSocketsDo $ do
  socket <- liftIO $ listenOn portNum
  threadId <- liftIO forkIO $ socketHandler socket
  return $ PongServer threadId
  where
    portNum = PortNumber . fromIntegral $ pongPortNum
    pongPortNum = pongPort cfg

    socketHandler :: (MonadIO m) => Socket -> m ()
    socketHandler sock = finally (socketHandlerLoop sock) (sClose sock)

    socketHandlerLoop :: (MonadIO m) => Socket -> m ()
    socketHandlerLoop sock = do
      (handle, _, _) <- accept sock
      _ <- forkFinally (body handle) (const $ hClose handle)
      socketHandlerLoop sock

    body :: (MonadIO m) => Handle -> m ()
    body handle = do
      hSetBuffering handle NoBuffering
      msg <- pongMessage cfg
      C.hPutStr handle msg

stopServer :: (MonadIO m) => PongServer -> m ()
-- ^ Implementation of actually stopping the server
stopServer (PongServer threadId) = liftIO $ killThread threadId
