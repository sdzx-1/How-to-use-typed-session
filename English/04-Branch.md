# 04-Branch

In this chapter we will add branches to the protocol so that the protocol will automatically end after a certain number of loops. Let's get started.
The code for Chapter 4 can be found [here](https://github.com/sdzx-1/typed-session-tutorial/tree/04-Branch).

Remember in the first chapter we defined a type in Type.hs: PingPongBranchSt, which is exactly for branches. Let's modify it first:
```diff
diff --git a/src/Type.hs b/src/Type.hs
index 8c2c69f..3251853 100644
--- a/src/Type.hs
+++ b/src/Type.hs
@@ -9,7 +9,7 @@ import TypedSession.TH (protocol)
 data PingPongRole = Client | Server | Counter
   deriving (Show, Eq, Ord, Enum, Bounded)
 
-data PingPongBranchSt = PingPongBranchSt
+data PingPongBranchSt = Continue | Finish
   deriving (Show, Eq, Ord, Enum, Bounded)
 
 pingpongProtocol :: QuasiQuoter

```
Two states are added here: Continue and Finish, which respectively indicate continuing the loop and ending the program.

Next, modify the content of the protocol in Protocol.hs:
```diff
 [pingpongProtocol|
 
 Label 0
-Msg Ping [] Client Server
-Msg Pong [] Server Client
-Msg Add [Int] Client Counter
-Goto 0
-
+Branch Client ChoiceNextAction {
+  BranchSt Continue []
+    Msg Ping [] Client Server
+    Msg Pong [] Server Client
+    Msg Add [Int] Client Counter
+    Goto 0
+  BranchSt Finish []
+    Msg ServerStop [] Client Server
+    Msg CounterStop [] Client Counter
+    Terminal
+}
 |]
```
The messages communicated in the previous chapter were placed in the Continue branch. The Finish branch contains the ending message.

The syntax of Branch is more complicated, so let me explain it in detail. The syntax of Branch is as follows:
```
Branch Role ResultType {
  BranchSt St1 []
    ....
  BranchSt St2 []
    ....
  BranchSt St3 []
    ....
  ....
}
```

In the process of multi-role communication, the meaning of Branch is: `Role` needs to dynamically determine its next state at runtime, which often represents the existence of a function. ResultType is a singleton type with an uncertain state. St1 St2 St3... will generate its constructor. The following is the type that will be generated in the end:
```Haskell
data ResultType :: Protocol -> Type where
  BranchSt_St1 :: ResultType (..)
  BranchSt_St2 :: ResultType (..)
  BranchSt_St3 :: ResultType (..)
  ....
```

Our protocol is as follows:
```
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
```

Branch will generate the following types and functions behind the scenes:
```haskell
    data ChoiceNextAction (a_a6P8 :: PingPong)
      where
        BranchSt_Finish :: ChoiceNextAction ('S1 'Finish)
        BranchSt_Continue :: ChoiceNextAction ('S1 'Continue)

    type ChoiceNextActionFun (m_a6P9 :: Type -> Type) =
        Peer PingPongRole PingPong Client m_a6P9 ChoiceNextAction 'S0
```

Let's look at the new PingPong.prot file:
```
-------------------------------------Client--------------Server--------------Counter-------------
Label 0                                   (S0)                (S1 s)              (S2 s)         
  [Branch Client ChoiceNextAction]        (S0)                (S1 s)              (S2 s)         
  * BranchSt_Continue []             
  Ping []                            Send (S1 Continue)  Recv (S1 s)              (S2 s)         
    Pong []                          Recv (S3)           Send (S3)                (S2 s)         
    Add [Int]                        Send (S2 Continue)       (S1 s)         Recv (S2 s)         
    Goto 0                                (S0)                (S1 s)              (S2 s)         
  * BranchSt_Finish []               
  ServerStop []                      Send (S1 Finish)    Recv (S1 s)              (S2 s)         
    CounterStop []                   Send (S2 Finish)         (End)          Recv (S2 s)         
    Terminal                              (End)               (End)               (End)          

```

`ChoiceNextActionFun` represents the function type of the Client that changes the state from `S0` to the uncertain state `S1 Finish`, `S1 Continue`. So let's implement this function first:
```diff
+choice :: Int -> ChoiceNextActionFun IO
+choice i = do
+  if i == 100
+    then liftConstructor BranchSt_Finish
+    else liftConstructor BranchSt_Continue
```
The meaning here is that when `i` is equal to 100, the state is converted to `S1 Finish` (`BranchSt_Finish :: ChoiceNextAction ('S1 'Finish)`),
otherwise it is converted to `S1 Continue` (`BranchSt_Continue :: ChoiceNextAction ('S1 'Continue)`). It should be noted that this is a type of breakpoint, and the result of dynamic operation is required to determine the next state, so you need to ensure that the business logic here is correct.

Next, modify the clientPeer function:
```diff
-clientPeer :: Peer PingPongRole PingPong Client IO (At () (Done Client)) S0
-clientPeer = I.do
-  yield Ping
-  Pong <- await
-  yield (Add 1)
-  clientPeer
+clientPeer :: Int -> Peer PingPongRole PingPong Client IO (At () (Done Client)) S0
+clientPeer i = I.do
+  choice i I.>>= \case
+    BranchSt_Continue -> I.do
+      yield Ping
+      Pong <- await
+      yield (Add 1)
+      clientPeer (i + 1)
+    BranchSt_Finish -> I.do
+      yield ServerStop
+      yield CounterStop
+      returnAt ()
```
The clientPeer adds a parameter to record the number of loops, uses the choice function to determine the next action, and puts the previous communication logic into the BranchSt_Continue branch. In the BranchSt_Finish branch, the communication end logic is processed: ServerStop and CounterStop messages are sent to Server and Counter respectively, and the program ends.

Modify the serverPeer function:
```diff
-serverPeer :: Peer PingPongRole PingPong Server IO (At () (Done Server)) S0
+serverPeer :: Peer PingPongRole PingPong Server IO (At () (Done Server)) (S1 s)
 serverPeer = I.do
-  Ping <- await
-  yield Pong
-  serverPeer
+  await I.>>= \case
+    Ping -> I.do
+      yield Pong
+      serverPeer
+    ServerStop -> returnAt ()
```
Note that its initial state changes and it needs to receive the ServerStop message.

Modify the counterPeer function to be similar to serverPeer:
```diff
-counterPeer :: Int -> Peer PingPongRole PingPong Counter IO (At Int (Done Counter)) S1
+counterPeer :: Int -> Peer PingPongRole PingPong Counter IO (At Int (Done Server)) (S2 s)
 counterPeer val = I.do
   liftm $ putStrLn $ "Counter val is: " ++ show val
-  Add i <- await
-  counterPeer (val + i)
+  await I.>>= \case
+    Add i -> counterPeer (val + i)
+    CounterStop -> returnAt val
```
The initial state changes and the CounterStop message needs to be received.

Finally, modify the startup function of clientPeer:
```diff
--- a/src/Run.hs
+++ b/src/Run.hs
@@ -57,7 +57,7 @@ runTCPClient = withSocketsDo $ do
             ]
         clientDriver = driverSimple (myTracer "client: ") encodeMsg sendMap clientTvar id
 
-    void $ runPeerWithDriver clientDriver clientPeer
+    void $ runPeerWithDriver clientDriver (clientPeer 0)
     killThread thid1
     killThread thid2
 
```

Let's run the program using `cabal run server`, `cabal run counter` and `cabal run client`. The server and counter should be started before the client. The running results are as follows:
![run](../data/04-run.gif)
The results were exactly what we expected.

Next, I will introduce the profound impact of Branch on multi-role communication.
Using Branch means that at this moment, a certain role A needs to dynamically determine its next state. At the same time, other roles have entered an uncertain state, and they all need to wait for the message sent by A to determine their own state. For roles other than A, they cannot send any messages when they are in an uncertain state, and they cannot enter other Branch states. Therefore, for A, the first message in each branch of Branch must be sent by A to other roles. Of course, our state generator will check whether these conditions are met. We use the variable `s` to represent that the role is in an uncertain state.
```
-------------------------------------Client--------------Server--------------Counter-------------
Label 0                                   (S0)                (S1 s)              (S2 s)         
  [Branch Client ChoiceNextAction]        (S0)                (S1 s)              (S2 s)         
  * BranchSt_Continue []             
  Ping []                            Send (S1 Continue)  Recv (S1 s)              (S2 s)         
    Pong []                          Recv (S3)           Send (S3)                (S2 s)         
    Add [Int]                        Send (S2 Continue)       (S1 s)         Recv (S2 s)         
    Goto 0                                (S0)                (S1 s)              (S2 s)         
  * BranchSt_Finish []               
  ServerStop []                      Send (S1 Finish)    Recv (S1 s)              (S2 s)         
    CounterStop []                   Send (S2 Finish)         (End)          Recv (S2 s)         
    Terminal                              (End)               (End)               (End)          

```
Branch also affects Terminal. Using Terminal in any branch requires all roles to be in a certain state. Taking the protocol in this chapter as an example, if you do not send CounterStop message to Counter, Counter will wait forever, but at the same time, Client and Server have stopped and they will not send any message to Counter. This will cause Counter to wait forever. The state generator will also check whether such an error occurs.

Finally, the existence of Branch makes the design of the `loop` use a combination of `Label` and `Goto`.

[Next Chapter 05-Random](05-Random.md)