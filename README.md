Haskell Pong Server for Easy Pinging
=====================================

For cloud-deployed apps, you often want to be able to monitor for the liveness and responsiveness of a particular process on the virtual server. There are some cute
ways to do this that involve monitoring the process list, as well as checking for CPU and other resource usage. This library enables a simpler approach: run a TCP
server that you can connect to and "ping" (in the loosest sense of the word "ping"). By default, the server runs on 10411 and just responds with the four characters
"pong" in the system encoding. You can configure the port to any number you would like, and you can make the server say anything you want by passing in an action
of the type `IO ByteString`.

Synopsis
----------

```haskell
import Network.Pong

main :: IO ()
main = withPongServer defaultPongConfig $ do
  -- your code goes here
```
