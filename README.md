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

Documentation
---------------

See the Haddock.

Help Wanted
-------------

I would very much like `pongMessage` to be an unbound monad, and to assume that the user is existing within a monad transformer stack.
Unfortunately, I get into a slight problem with trying to execute `pongMessage` in one stack while returning `IO`. I am pretty sure
this is why `MonadBaseControl` exists, but I get type errors in trying to use it. I'd love to have a pair programming session with
someone to work through this issue.  Failing that, I'll happily take a pull request.
