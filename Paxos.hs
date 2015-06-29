{-# LANGUAGE RebindableSyntax #-}

module Paxos (paxos) where

import Language.Copilot
import qualified Prelude as P

-- message types (constant streams)
noMsg, prepare, promise, accept, acknowledge :: Stream Word8
(noMsg, prepare, promise, accept, acknowledge) = (0, 1, 2, 3, 4)

-- possible statuses (constant streams)
idle, trying, polling :: Stream Word8
(idle, trying, polling) = (0, 1, 2)

yesnono :: Stream Bool
yesnono = [True] ++ false

-- number of parallel processes (majority of which are needed for consensus)
nReplicas :: Stream Int32
nReplicas = 4

paxos :: Spec
paxos = do
    -- request that a message be sent
    trigger "send_trigger" (sendMsgType /= noMsg) [arg sendTo, arg sendMsgType, arg sendN, arg sendLastN, arg sendTemp]
    -- report success
    trigger "success_trigger" success [arg successTemp]
    where
    -- external streams
    -- temperature read from the sensor
    temp        = externD "temp" $ Just [36.6..]
    -- type of the received message (possibly noMsg)
    recvMsgType = externW8 "recv_message_type" $ Just $ repeat 0
    -- sender id (between 0 and nReplicas-1)
    recvFrom    = externW8 "recv_from" $ Just $ repeat 0
    -- ballot number concerned
    recvN       = externI32 "recv_n" $ Just $ repeat 0
    -- only meaningful for promise messages:
    -- highest ballot number acknowledged by the sender (-1 if no acks)
    recvLastN   = externI32 "recv_last_n" $ Just $ repeat 0
    -- promise: temperature of ballot recvLastN,
    -- accept: temperature that we're trying to get everyone to agree on
    recvTemp    = externD "recv_temp" $ Just $ repeat 0
    -- a number between 0 and nReplicas-1 (constant stream)
    myId        = externW8 "my_id" $ Just $ repeat 0
    -- stream of random doubles between 0 and 1
    rand01      = externD "rand01" $ Just $ P.cycle [0.15, 0.05, 0.95, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25]
    -- stream with common offset for the entire group
    offset      = myId - (myId `mod` 4)

    -- arguments for send_trigger (meanings analogous to those of recv* above)
    sendMsgType :: Stream Word8
    sendMsgType = if wannaPrepare
        then prepare
        else if wannaPromise
            then promise
            else if wannaPoll
                then accept
                else if wannaAck
                    then acknowledge
                    else noMsg

    sendN :: Stream Int32
    sendN = if wannaPrepare
        then myLastPreparedN
        else recvN

    sendTo :: Stream Word8
    sendTo = recvFrom

    sendLastN :: Stream Int32
    sendLastN = myLastAckN

    sendTemp :: Stream Double
    sendTemp = if wannaPoll then myTemp else myLastAckTemp

    -- consensus reached (acks have been received from a majority)
    success :: Stream Bool
    success = myAckers 0 && myAckers 1 && myAckers 2
        || myAckers 0 && myAckers 1 && myAckers 3
        || myAckers 0 && myAckers 2 && myAckers 3
        || myAckers 1 && myAckers 2 && myAckers 3

    -- consensus value
    successTemp :: Stream Double
    successTemp = myTemp

    -- ballot number of my last (or current) prepare message
    myLastPreparedN, laggingLastPreparedN :: Stream Int32
    myLastPreparedN = if wannaPrepare
        then nextN
        else laggingLastPreparedN
    laggingLastPreparedN = if yesnono then cast myId else [-1] ++ myLastPreparedN

    -- helper streams for calculating next ballot number
    nextN :: Stream Int32
    nextN = nReplicas + if laggingLastPreparedN > candidateN
        then laggingLastPreparedN
        else candidateN

    candidateN :: Stream Int32
    candidateN = myLastPromiseN - myLastPromiseN `mod` nReplicas + cast myId

    -- ballot number from my last (or current) acknowledge message
    myLastAckN, laggingLastAckN :: Stream Int32
    myLastAckN = if wannaAck then recvN else laggingLastAckN
    laggingLastAckN = [-1] ++ myLastAckN

    -- ...and associated value
    myLastAckTemp, laggingLastAckTemp :: Stream Double
    myLastAckTemp = if wannaAck then recvTemp else laggingLastAckTemp
    laggingLastAckTemp = [0] ++ myLastAckTemp

    -- ballot number from my last (or current) promise message
    myLastPromiseN, laggingLastPromiseN :: Stream Int32
    myLastPromiseN = if wannaPromise then recvN else laggingLastPromiseN
    laggingLastPromiseN = [-1] ++ myLastPromiseN

    -- current status: idle, trying or polling
    myStatus, laggingStatus :: Stream Word8
    myStatus = if wannaPrepare
        then trying
        else if wannaPoll
            then polling
            else laggingStatus
    laggingStatus = [0] ++ myStatus

    -- true if I have received a promise in response to my prepare
    myPromisors0, laggingPromisors0 :: Stream Bool
    myPromisors1, laggingPromisors1 :: Stream Bool
    myPromisors2, laggingPromisors2 :: Stream Bool
    myPromisors3, laggingPromisors3 :: Stream Bool
    myPromisors0 = if wannaPrepare
        then false
        else if validPromise && recvFrom == (0 + offset)
            then true
            else laggingPromisors0
    laggingPromisors0 =  [False] ++ myPromisors0
    myPromisors1 = if wannaPrepare
        then false
        else if validPromise && recvFrom == (1 + offset)
            then true
            else laggingPromisors1
    laggingPromisors1 =  [False] ++ myPromisors1
    myPromisors2 = if wannaPrepare
        then false
        else if validPromise && recvFrom == (2 + offset)
            then true
            else laggingPromisors2
    laggingPromisors2 =  [False] ++ myPromisors2
    myPromisors3 = if wannaPrepare
        then false
        else if validPromise && recvFrom == (3 + offset)
            then true
            else laggingPromisors3
    laggingPromisors3 =  [False] ++ myPromisors3

    validPromise, betterPromise :: Stream Bool
    validPromise = laggingStatus == trying && recvMsgType == promise && recvN == laggingLastPreparedN
    betterPromise = validPromise && recvLastN > laggingBestPromisedN

    validAck :: Stream Bool
    validAck = laggingStatus == polling && recvMsgType == acknowledge && recvN == laggingLastPreparedN

    -- highest ballot number received in a promise message
    myBestPromisedN, laggingBestPromisedN :: Stream Int32
    myBestPromisedN = if wannaPrepare
        then (-1)
        else if betterPromise
            then recvLastN
            else laggingBestPromisedN
    laggingBestPromisedN = [-1] ++ myBestPromisedN

    -- associated temperature
    myBestPromisedTemp, laggingBestPromisedTemp :: Stream Double
    myBestPromisedTemp = if betterPromise
            then recvTemp
            else laggingBestPromisedTemp
    laggingBestPromisedTemp = [0] ++ myBestPromisedTemp

    myTemp, laggingTemp :: Stream Double
    myTemp = if wannaPoll
        then if myBestPromisedN >= 0
            then myBestPromisedTemp
            else temp
        else laggingTemp
    laggingTemp = [0] ++ myTemp

    -- true if I have received an acknowledge message in response to my accept
    myAckers, laggingAckers :: Word8 -> Stream Bool
    myAckers i = if wannaPoll
        then false
        else if validAck && recvFrom == (constant i + offset)
            then true
            else laggingAckers  i
    laggingAckers i = [False] ++ myAckers i

    -- true when it's a good idea to send a particular type of message
    wannaPrepare, wannaPromise, wannaPoll, wannaAck :: Stream Bool
    wannaPromise = recvMsgType == prepare && recvN > laggingLastPromiseN
    wannaPoll = validPromise &&
        (myPromisors0 && myPromisors1 && myPromisors2
        || myPromisors0 && myPromisors1 && myPromisors3
        || myPromisors0 && myPromisors2 && myPromisors3
        || myPromisors1 && myPromisors2 && myPromisors3)
    wannaAck = recvMsgType == accept && recvN == laggingLastPromiseN
    wannaPrepare = recvMsgType == noMsg && rand01 < 0.1
