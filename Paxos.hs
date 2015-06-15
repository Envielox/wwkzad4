{-# LANGUAGE RebindableSyntax #-}

module Paxos (paxos) where

import Language.Copilot
import qualified Prelude as P

noMsg, prepare, promise, accept, acknowledge :: Word8
noMsg : prepare : promise : accept : acknowledge : _ = [0..]

idle, trying, polling :: Word8
idle : trying : polling : _ = [0..]

yesnono :: Stream Bool
yesnono = [True] ++ false

nReplicas :: Int32
nReplicas = 4

paxos :: Spec
paxos = do
    trigger "send_trigger" (sendMsgType /= constant noMsg) [arg sendTo, arg sendMsgType, arg sendN, arg sendLastN, arg sendTemp]
    trigger "success_trigger" success [arg successTemp, arg successHum]
    where
    -- external streams
    temp        = externD "temp" $ Just [36.6..]
    hum         = externD "hum" Nothing
    recvMsgType = externW8 "recv_message_type" $ Just $ repeat noMsg
    recvFrom    = externW8 "recv_from" Nothing
    recvN       = externI32 "recv_n" Nothing
    recvLastN   = externI32 "recv_last_n" Nothing
    recvTemp    = externD "recv_temp" Nothing
    myId        = externW8 "my_id" $ Just $ repeat 0
    rand01      = externD "rand01" $ Nothing

    -- for now
    successHum :: Stream Double
    successHum = successTemp

    sendMsgType :: Stream Word8
    sendMsgType = if wannaPrepare
        then constant prepare
        else if wannaPromise
            then constant promise
            else if wannaPoll
                then constant accept
                else if wannaAck
                    then constant acknowledge
                    else constant noMsg

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

    success :: Stream Bool
    success = myAckers0 && myAckers1 && myAckers2
        || myAckers0 && myAckers1 && myAckers3
        || myAckers0 && myAckers2 && myAckers3
        || myAckers1 && myAckers2 && myAckers3

    successTemp :: Stream Double
    successTemp = myTemp

    myLastPreparedN, laggingLastPreparedN :: Stream Int32
    myLastPreparedN = if wannaPrepare
        then laggingLastPreparedN + constant nReplicas
        else laggingLastPreparedN
    laggingLastPreparedN = if yesnono then cast myId else [-1] ++ myLastPreparedN

    myLastAckN, laggingLastAckN :: Stream Int32
    myLastAckN = if wannaAck then recvN else laggingLastAckN
    laggingLastAckN = [-1] ++ myLastAckN

    myLastAckTemp, laggingLastAckTemp :: Stream Double
    myLastAckTemp = if wannaAck then recvTemp else laggingLastAckTemp
    laggingLastAckTemp = [0] ++ myLastAckTemp

    myLastPromiseN, laggingLastPromiseN :: Stream Int32
    myLastPromiseN = if wannaPromise then recvN else laggingLastPromiseN
    laggingLastPromiseN = [-1] ++ myLastPromiseN

    myStatus, laggingStatus :: Stream Word8
    myStatus = if wannaPrepare
        then constant trying
        else if wannaPoll
            then constant polling
            else laggingStatus
    laggingStatus = [idle] ++ myStatus

    myPromisors0, laggingPromisors0 :: Stream Bool
    myPromisors1, laggingPromisors1 :: Stream Bool
    myPromisors2, laggingPromisors2 :: Stream Bool
    myPromisors3, laggingPromisors3 :: Stream Bool
    myPromisors0 = if wannaPrepare
        then false
        else if validPromise && recvFrom == constant 0
            then true
            else laggingPromisors0
    laggingPromisors0 =  [False] ++ myPromisors0
    myPromisors1 = if wannaPrepare
        then false
        else if validPromise && recvFrom == constant 1
            then true
            else laggingPromisors1
    laggingPromisors1 =  [False] ++ myPromisors1
    myPromisors2 = if wannaPrepare
        then false
        else if validPromise && recvFrom == constant 2
            then true
            else laggingPromisors2
    laggingPromisors2 =  [False] ++ myPromisors2
    myPromisors3 = if wannaPrepare
        then false
        else if validPromise && recvFrom == constant 3
            then true
            else laggingPromisors3
    laggingPromisors3 =  [False] ++ myPromisors3

    validPromise, betterPromise :: Stream Bool
    validPromise = laggingStatus == constant trying && recvMsgType == constant promise && recvN == laggingLastPreparedN
    betterPromise = validPromise && recvLastN > laggingBestPromisedN

    validAck :: Stream Bool
    validAck = laggingStatus == constant polling && recvMsgType == constant acknowledge && recvN == laggingLastPreparedN

    myBestPromisedN, laggingBestPromisedN :: Stream Int32
    myBestPromisedN = if wannaPrepare
        then constant (-1)
        else if betterPromise
            then recvLastN
            else laggingBestPromisedN
    laggingBestPromisedN = [-1] ++ myBestPromisedN

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

    myAckers0, laggingAckers0 :: Stream Bool
    myAckers1, laggingAckers1 :: Stream Bool
    myAckers2, laggingAckers2 :: Stream Bool
    myAckers3, laggingAckers3 :: Stream Bool
    myAckers0 = if wannaPoll
        then false
        else if validAck && recvFrom == constant 0
            then true
            else laggingAckers0
    laggingAckers0 = [False] ++ myAckers0
    myAckers1 = if wannaPoll
        then false
        else if validAck && recvFrom == constant 1
            then true
            else laggingAckers1
    laggingAckers1 = [False] ++ myAckers1
    myAckers2 = if wannaPoll
        then false
        else if validAck && recvFrom == constant 2
            then true
            else laggingAckers2
    laggingAckers2 = [False] ++ myAckers2
    myAckers3 = if wannaPoll
        then false
        else if validAck && recvFrom == constant 3
            then true
            else laggingAckers3
    laggingAckers3 = [False] ++ myAckers3

    wannaPrepare, wannaPromise, wannaPoll, wannaAck :: Stream Bool
    wannaPromise = recvMsgType == constant prepare && recvN > laggingLastPromiseN
    wannaPoll = validPromise &&
        (myPromisors0 && myPromisors1 && myPromisors2
        || myPromisors0 && myPromisors1 && myPromisors3
        || myPromisors0 && myPromisors2 && myPromisors3
        || myPromisors1 && myPromisors2 && myPromisors3)
    wannaAck = recvMsgType == constant accept && recvN == laggingLastPromiseN
    wannaPrepare = recvMsgType == constant noMsg && rand01 < 0.1

    {-
    -- pseudocode

    -- state
    myLastPreparedN := whatever
    myLastAckN := -1 (aka Nothing)
    myLastAckTemp := whatever
    myLastPromiseN := -1
    myStatus := idle
    myPromisors := whatever
    myBestPromisedN := whatever
    myBestPromisedTemp := whatever
    myTemp := whatever
    myAckers := whatever

    -- nothing received, no success, considering prepare
    wannaPrepare = nothingBetterToDo && random?
    myStatus := trying
    myVotes := empty
    myLastPreparedN := generate
    sendTo := whatever
    sendMsgType := prepare
    sendN := myLastPreparedN
    send* := whatever
    myBestPromisedN := Nothing
    myBestPromisedTemp := Nothing

    -- prepare received
    recvN <= myLastPromiseN -> doNothing
    recvN > myLastPromiseN:
    myLastPromiseN := recvN
    sendMsgType := promiseAnything/promiseLast
    sendTo := recvFrom
    sendN := recvN
    sendLastN := myLastAckN/empty
    sendTemp := myLastAckTemp/empty

    -- promise received
    recvN /= myLastPreparedN || myStatus /= trying -> doNothing
    otherwise:
    myPromisors <>= [recvFrom]
    if recvLastN > myBestPromisedN:
        myBestPromisedN
        myBestPromisedTemp := recvTemp
    if size myPromisors < quorum:
        sendMsgType := noMsg
        send* := whatever
    else:
        myStatus := polling
        myAckers := empty
        myTemp := myBestPromisedTemp/temp
        -- can we send this to people that haven't responded with a promise? below i assume we can, not sure though
        sendMsgType := accept
        sendN := myLastPreparedN
        sendTemp := myTemp
        send* := whatever

    -- accept received
    recvN /= myLastPromiseN -> doNothing
    otherwise:
    myLastAckN := recvN
    myLastAckTemp := recvTemp
    sendMsgType := acknowledge
    sendTo := recvFrom
    sendN := recvN
    send* := whatever

    -- acknowledge received
    recvN /= myLastPreparedN || myStatus /= polling -> doNothing
    otherwise:
    myAckers <>= [recvFrom]
    sendMsgType := noMsg
    send* := whatever
    if size myAckers >= quorum:
        success := True
        successTemp := myTemp
    -}
