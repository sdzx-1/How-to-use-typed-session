# 06-Check
The code for Chapter 6 can be found [here](https://github.com/sdzx-1/typed-session-tutorial/tree/06-Check).

Now let's revisit the protocol so far:
```
[pingpongProtocol|

Label 0
Branch Client ChoiceNextAction {
  BranchSt Continue []
    Msg Ping [] Client Server
    Msg Pong [] Server Client
    Msg Add [Int] Client Counter
    Goto 0
  BranchSt Finish []
    Msg ServerStop [] Client Server
    Msg CounterStop [] Client Counter
    Terminal
}
|]
```
Note that in our protocol, the Client sends Add messages to the Counter in a loop to let the Counter accumulate values, but the Counter never sends any messages to the Client.

Therefore, the Client does not know whether the Counter has completed the accumulation work correctly.

Of course, we can require the Counter to send a message to the Client to inform the Client of the accumulation result after each accumulation is completed, but this will increase the communication once and also turn the almost asynchronous communication between the Client and the Counter into synchronous communication (the Client must wait for the Counter's response), which will greatly affect the efficiency of data transmission between them.

So we use a new idea here:

The Client periodically sends CheckVal messages to the Counter. CheckVal carries the current accumulated value of the Client. After receiving the CheckVal message, the Counter checks whether the value it carries is equal to its own accumulated value.

If they are equal, Counter returns a CheckSuccessed message to Client and continues to wait for the next message.

If they are not equal, Counter returns a CheckFailed message to Client, which contains the reason for the error, and then Counter ends the communication.

At the same time, after receiving the CheckFailed message, the Client will send a CheckErrorHappened message to the Server, and the Client will end the communication. The Server will end the communication after receiving the CheckErrorHappened message.

Based on the above ideas, we started to modify the code.

First modify the PingPongBranchSt of the Type.hs file:
```diff
--- a/src/Type.hs
+++ b/src/Type.hs
@@ -9,7 +9,7 @@ import TypedSession.TH (protocol)
 data PingPongRole = Client | Server | Counter
   deriving (Show, Eq, Ord, Enum, Bounded)
 
-data PingPongBranchSt = Continue | Finish
+data PingPongBranchSt = Continue | Finish | Check | Successed | Failed
   deriving (Show, Eq, Ord, Enum, Bounded)
 
 pingpongProtocol :: QuasiQuoter
```
It adds three states: Check, Successed, Failed.

Then modify the Protocol.hs file:
First modify the protocol
```diff
--- a/src/Protocol.hs
+++ b/src/Protocol.hs
@@ -38,6 +38,17 @@ Branch Client ChoiceNextAction {
     Msg ServerStop [] Client Server
     Msg CounterStop [] Client Counter
     Terminal
+  BranchSt Check []
+    Msg CheckVal [Int] Client Counter
+    Branch Counter CheckResult {
+      BranchSt Successed []
+        Msg CheckSuccessed [] Counter Client
+        Goto 0
+      BranchSt Failed []
+        Msg CheckFailed [String] Counter Client
+        Msg CheckErrorHappened [String] Client Server
+        Terminal
+    }
 }
 |]
```
Added BranchSt Check in Client ChoiceNextAction. Its meaning is the same as above, so I won't repeat it here.

Added new messages, need to update the codec:
```diff
index 20296db..7970584 100644
 instance Show (AnyMsg PingPongRole PingPong) where
@@ -47,6 +58,10 @@ instance Show (AnyMsg PingPongRole PingPong) where
     Add i -> "Add " <> show i
     ServerStop -> "ServerStop"
     CounterStop -> "CounterStop"
+    CheckVal i -> "CheckVal " <> show i
+    CheckSuccessed -> "CheckSuccessed"
+    CheckFailed st -> "CheckFailed " <> st
+    CheckErrorHappened st -> "CheckErrorHappened" <> st
 
 encodeMsg :: Encode PingPongRole PingPong L.ByteString
 encodeMsg = Encode $ \x -> runPut $ case x of
@@ -55,6 +70,10 @@ encodeMsg = Encode $ \x -> runPut $ case x of
   Add i -> putWord8 2 >> put i
   ServerStop -> putWord8 3
   CounterStop -> putWord8 4
+  CheckVal i -> putWord8 5 >> put i
+  CheckSuccessed -> putWord8 6
+  CheckFailed st -> putWord8 7 >> put st
+  CheckErrorHappened st -> putWord8 8 >> put st
 
 getAnyMsg :: Get (AnyMsg PingPongRole PingPong)
 getAnyMsg = do
@@ -67,6 +86,16 @@ getAnyMsg = do
       return $ AnyMsg $ Add i
     3 -> return $ AnyMsg ServerStop
     4 -> return $ AnyMsg CounterStop
+    5 -> do
+      i <- get
+      return $ AnyMsg $ CheckVal i
+    6 -> return $ AnyMsg CheckSuccessed
+    7 -> do
+      st <- get
+      return $ AnyMsg (CheckFailed st)
+    8 -> do
+      st <- get
+      return $ AnyMsg (CheckErrorHappened st)
     _ -> fail "Invalid message tag"
 
```

Next, we need to modify the specific communication code in the Peer.hs file.
First, the choice function:
```diff
 {-# LANGUAGE LambdaCase #-}
+{-# LANGUAGE MultiWayIf #-}
 {-# LANGUAGE QualifiedDo #-}

 choice :: Int -> ChoiceNextActionFun IO
 choice i = do
-  if i == 100
-    then liftConstructor BranchSt_Finish
-    else liftConstructor BranchSt_Continue
+  if
+    | i == 101 -> liftConstructor BranchSt_Finish
+    | i == 100 -> liftConstructor BranchSt_Check
+    | i `mod` 10 == 0 -> liftConstructor BranchSt_Check
+    | otherwise -> liftConstructor BranchSt_Continue
```
We check once every 10 loops. Previously, we ended directly when i == 100, but we want to check before ending, so we also check once when i == 100 and end when i == 101. This is a trick that we can do when we want to check before ending.

Next, modify the clientPeer function:
```diff
 clientPeer
   :: Int
   -> IORef Int
-  -> Peer PingPongRole PingPong Client IO (At Int (Done Client)) S0
+  -> Peer PingPongRole PingPong Client IO (At (Either String Int) (Done Client)) S0
 clientPeer i valRef = I.do
   choice i I.>>= \case
     BranchSt_Continue -> I.do
@@ -39,19 +42,51 @@ clientPeer i valRef = I.do
       yield ServerStop
       yield CounterStop
       At val <- liftm $ readIORef valRef
-      returnAt val
+      returnAt (Right val)
+    BranchSt_Check -> I.do
+      At val <- liftm $ readIORef valRef
+      yield (CheckVal val)
+      await I.>>= \case
+        CheckSuccessed -> clientPeer (i + 1) valRef
+        CheckFailed st -> I.do
+          yield (CheckErrorHappened st)
+          returnAt (Left st)
```
Since the check may fail, the output type becomes (Either String Int), and the rest is a direct translation of the previous idea.

Next, modify the serverPeer function:
```diff

-serverPeer :: Peer PingPongRole PingPong Server IO (At () (Done Server)) (S1 s)
+serverPeer :: Peer PingPongRole PingPong Server IO (At (Either String ()) (Done Server)) (S1 s)
 serverPeer = I.do
   await I.>>= \case
     Ping -> I.do
       yield Pong
       serverPeer
-    ServerStop -> returnAt ()
+    ServerStop -> returnAt (Right ())
+    CheckErrorHappened st -> returnAt (Left st)
```
Since the check may fail, the output type becomes (Either String ()).

At the same time, the startup code of serverPeer needs to be modified to print the output result:
```diff
+++ b/src/Run.hs
@@ -65,7 +65,9 @@ runTCPClient = withSocketsDo $ do
     putStrLn $ "Client val is: " <> show res
 
 runTCPServer :: IO ()
-runTCPServer = runTCPServer' Nothing "3000" "Server" SClient serverPeer
+runTCPServer = do
+  val <- runTCPServer' Nothing "3000" "Server" SClient serverPeer
+  putStrLn $ "Server val is: " <> show val
```

Next, implement the checkFun function, which Counter uses to dynamically determine the next state:
```diff
+checkFun :: Int -> Int -> CheckResultFun IO
+checkFun val ci =
+  if val == ci
+    then liftConstructor BranchSt_Successed
+    else liftConstructor BranchSt_Failed
```

Finally, modify the counterPeer function:
```diff
-counterPeer :: Int -> Peer PingPongRole PingPong Counter IO (At Int (Done Server)) (S2 s)
+counterPeer :: Int -> Peer PingPongRole PingPong Counter IO (At (Either String Int) (Done Server)) (S2 s)
 counterPeer val = I.do
   liftm $ putStrLn $ "Counter val is: " ++ show val
   await I.>>= \case
-    Add i -> counterPeer (val + i)
-    CounterStop -> returnAt val
+    Add i -> I.do
+      At rval <- liftm $ randomRIO @Int (0, 1000)
+      -- 0.995^100 ~= 0.6
+      let nval = val + if rval < 995 then i else (i - 1)
+      counterPeer nval
+    CounterStop -> returnAt (Right val)
+    CheckVal ci -> I.do
+      checkFun val ci I.>>= \case
+        BranchSt_Successed -> I.do
+          yield CheckSuccessed
+          counterPeer val
+        BranchSt_Failed -> I.do
+          let reason =
+                "Check failed, now value is "
+                  <> show val
+                  <> ", check value is "
+                  <> show ci
+          yield (CheckFailed reason)
+          returnAt (Left reason)
```
Since the check may fail, the output type becomes (Either String Int). To make the example more interesting, we let the value accumulated by Counter decrease by 1 with a probability of 5/1000, so that the accumulated value may be wrong each time, thus triggering the branch of check failure. Note the use of the checkFun function here, and the rest is a direct translation of the previous idea.

Let's run the program using `cabal run server`, `cabal run counter` and `cabal run client`. The server and counter should be started before the client. The running results are as follows:
![run](../data/06-run.gif)
Now the result has a certain probability of failure, and when it fails, each character will print the reason for the failure.

Looking at the current protocol code:
```haskell
[pingpongProtocol|

Label 0
Branch Client ChoiceNextAction {
  BranchSt Continue []
    Msg Ping [] Client Server
    Msg Pong [] Server Client
    Msg Add [Int] Client Counter
    Goto 0
  BranchSt Finish []
    Msg ServerStop [] Client Server
    Msg CounterStop [] Client Counter
    Terminal
  BranchSt Check []
    Msg CheckVal [Int] Client Counter
    Branch Counter CheckResult {
      BranchSt Successed []
        Msg CheckSuccessed [] Counter Client
        Goto 0
      BranchSt Failed []
        Msg CheckFailed [String] Counter Client
        Msg CheckErrorHappened [String] Client Server
        Terminal
    }
}
|]
```
It has a certain degree of complexity, and `typed-session` + `hls` allows us to develop it incrementally and interactively. This greatly reduces our mental burden, and it also means that we can handle more complex protocols.

[Next Chapter 07-Fix](07-Fix.md)