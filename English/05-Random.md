# 05-Random
The code for Chapter 5 can be found [here](https://github.com/sdzx-1/typed-session-tutorial/tree/05-Random).

In this chapter, we try to add some services instead of just sending messages to each other. Our idea is to let the client randomly generate values ​​from 0 to 100, send the values ​​to the Counter through the Add message, and let the Counter accumulate these values. At the same time, the client also accumulates these values. Finally, let the client and the Counter output the accumulated results.

This chapter does not require modification of the communication protocol, only adding services to the communication statements.

First add the random dependency:
```diff
--- a/typed-session-tutorial.cabal
+++ b/typed-session-tutorial.cabal
@@ -78,6 +78,7 @@ library
                     , bytestring
                     , binary
                     , containers
+                    , random
 
     -- Directories containing source files.
     hs-source-dirs:   src
```

The counterPeer function conforms to the accumulation semantics and does not need to be modified, so only the clientPeer function needs to be modified:
```diff
-clientPeer :: Int -> Peer PingPongRole PingPong Client IO (At () (Done Client)) S0
-clientPeer i = I.do
+clientPeer
+  :: Int
+  -> IORef Int
+  -> Peer PingPongRole PingPong Client IO (At Int (Done Client)) S0
+clientPeer i valRef = I.do
   choice i I.>>= \case
     BranchSt_Continue -> I.do
       yield Ping
       Pong <- await
-      yield (Add 1)
-      clientPeer (i + 1)
+      At randVal <- liftm $ do
+        rval <- randomRIO @Int (0, 100)
+        modifyIORef valRef (+ rval)
+        pure rval
+      yield (Add randVal)
+      clientPeer (i + 1) valRef
     BranchSt_Finish -> I.do
       yield ServerStop
       yield CounterStop
-      returnAt ()
+      At val <- liftm $ readIORef valRef
+      returnAt val
```
An IORef is added to save the accumulated value. The main purpose of using IORef is to show how to insert ordinary IO operations into communication statements.
Here IO can be any other `Monad m`, depending on your business needs. Use liftm to convert ordinary IO operations to Peer. liftm does not change the state of the protocol, so you can use liftm anywhere to turn the internal m (here IO) into Peer.

The definition of liftm is as follows:
```haskell
{- |
Lift any m to Peer role' ps r m, which is an application of LiftM.
Note that the state of `ts` has not changed.
-}
liftm :: (Functor m) => m a -> Peer role' ps r m (At a ts) ts
liftm m = LiftM (returnAt <$> m)
```

Note that clientPeer will eventually return the accumulated result, so its startup function needs to be modified to print the final result:
```diff
index a19503a..c465bce 100644
--- a/src/Run.hs
+++ b/src/Run.hs
@@ -9,6 +9,7 @@ import qualified Control.Exception as E
 import Control.Monad (void)
 import Control.Monad.Class.MonadFork (forkIO, killThread)
 import Data.IFunctor (At)
+import Data.IORef
 import qualified Data.IntMap as IntMap
 import Network.Socket
 import Peer
@@ -57,9 +58,11 @@ runTCPClient = withSocketsDo $ do
             ]
         clientDriver = driverSimple (myTracer "client: ") encodeMsg sendMap clientTvar id
 
-    void $ runPeerWithDriver clientDriver (clientPeer 0)
+    valRef <- newIORef 0
+    res <- runPeerWithDriver clientDriver (clientPeer 0 valRef)
     killThread thid1
     killThread thid2
+    putStrLn $ "Client val is: " <> show res
 
 runTCPServer :: IO ()
 runTCPServer = runTCPServer' Nothing "3000" "Server" SClient serverPeer
```

Let's run the program using `cabal run server`, `cabal run counter` and `cabal run client`. The server and counter should be started before the client. The running results are as follows:
![run](../data/05-run.gif)

The accumulated results output by the client and the counter are the same, which is perfect.

[Next Chapter 06-Check](06-Check.md)