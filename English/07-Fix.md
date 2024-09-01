# 07-Fix
The code for Chapter 7 can be found [here](https://github.com/sdzx-1/typed-session-tutorial/tree/07-Fix).

In the previous chapter, we added the logic of periodic checking between Client and Counter. Failure of the check will directly lead to the termination of the protocol, and all roles will eventually terminate.

In this chapter, if the Counter check fails, the Client will resend the new value to it through the Fix message. After receiving the Fix message, the Counter will update the accumulated value and return the FixFinish message. This method is used to fix the error, and the process does not involve communication with the Server.

At this time, careful readers may wonder: Doesn't CheckVal contain the accumulated value? Can't it be directly updated when the check fails? This is because this is just an example. In the actual code, the check carries very little information, while the fix needs to carry more information. They are unlikely to be the same.

Let's start by modifying the protocol:
```diff
--- a/src/Protocol.hs
+++ b/src/Protocol.hs
@@ -46,8 +46,9 @@ Branch Client ChoiceNextAction {
         Goto 0
       BranchSt Failed []
         Msg CheckFailed [String] Counter Client
-        Msg CheckErrorHappened [String] Client Server
-        Terminal
+        Msg Fix [Int] Client Counter
+        Msg FixFinish [] Counter Client
+        Goto 0
     }
 }
 |]
```

Modify the corresponding codec:
```diff
index 7970584..23f3d9e 100644
@@ -61,7 +62,8 @@ instance Show (AnyMsg PingPongRole PingPong) where
     CheckVal i -> "CheckVal " <> show i
     CheckSuccessed -> "CheckSuccessed"
     CheckFailed st -> "CheckFailed " <> st
-    CheckErrorHappened st -> "CheckErrorHappened" <> st
+    Fix i -> "Fix val " <> show i
+    FixFinish -> "FixFinish"
 
 encodeMsg :: Encode PingPongRole PingPong L.ByteString
 encodeMsg = Encode $ \x -> runPut $ case x of
@@ -73,7 +75,8 @@ encodeMsg = Encode $ \x -> runPut $ case x of
   CheckVal i -> putWord8 5 >> put i
   CheckSuccessed -> putWord8 6
   CheckFailed st -> putWord8 7 >> put st
-  CheckErrorHappened st -> putWord8 8 >> put st
+  Fix st -> putWord8 8 >> put st
+  FixFinish -> putWord8 9
 
 getAnyMsg :: Get (AnyMsg PingPongRole PingPong)
 getAnyMsg = do
@@ -95,7 +98,8 @@ getAnyMsg = do
       return $ AnyMsg (CheckFailed st)
     8 -> do
       st <- get
-      return $ AnyMsg (CheckErrorHappened st)
+      return $ AnyMsg (Fix st)
+    9 -> return $ AnyMsg FixFinish
     _ -> fail "Invalid message tag"
```

Modify the clientPeer function:
```diff
@@ -48,9 +48,11 @@ clientPeer i valRef = I.do
       yield (CheckVal val)
       await I.>>= \case
         CheckSuccessed -> clientPeer (i + 1) valRef
-        CheckFailed st -> I.do
-          yield (CheckErrorHappened st)
-          returnAt (Left st)
+        (CheckFailed st) -> I.do
+          liftm $ putStrLn st
+          yield (Fix val)
+          FixFinish <- await
+          clientPeer i valRef
```

Modify the serverPeer function:
```diff
 serverPeer :: Peer PingPongRole PingPong Server IO (At (Either String ()) (Done Server)) (S1 s)
 serverPeer = I.do
@@ -59,7 +61,6 @@ serverPeer = I.do
       yield Pong
       serverPeer
     ServerStop -> returnAt (Right ())
-    CheckErrorHappened st -> returnAt (Left st)
```

Modify the counterPeer function:
```diff
 checkFun :: Int -> Int -> CheckResultFun IO
 checkFun val ci =
@@ -89,4 +90,6 @@ counterPeer val = I.do
                   <> ", check value is "
                   <> show ci
           yield (CheckFailed reason)
-          returnAt (Left reason)
+          (Fix newVal) <- await
+          yield FixFinish
+          counterPeer newVal
```

Let's run the program using `cabal run server`, `cabal run counter` and `cabal run client`. The server and counter should be started before the client. The running results are as follows:
![run](../data/07-run.gif)