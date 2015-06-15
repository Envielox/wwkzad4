{-# LANGUAGE RebindableSyntax #-}

module Paxos (paxos) where

import Language.Copilot
import qualified Prelude as P

noMsg, prepare, promise, accept, acknowledge :: Word8
noMsg : prepare : promise : accept : acknowledge : _ = [0..]

idle, trying, polling :: Word8
idle : trying : polling : _ = [0..]

quorum :: Int32
quorum = nReplicas `P.div` 2 P.+ 1

yesnono :: Stream Bool
yesnono = [True] ++ constB False

nReplicas :: Int32
nReplicas = 4

paxos :: Spec
paxos = do
    trigger "send_trigger" (sendMsgType /= constant noMsg) [arg sendMsgType, arg sendN, arg sendLastN, arg sendTemp]
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

    sendLastN :: Stream Int32
    sendLastN = myLastAckN

    sendTemp :: Stream Double
    sendTemp = if wannaPoll then myTemp else myLastAckTemp

    success :: Stream Bool
    success = undefined

    successTemp :: Stream Double
    successTemp = myTemp

    myLastPreparedN, laggingLastPreparedN :: Stream Int32
    myLastPreparedN = if wannaPrepare
        then laggingLastPreparedN + constant nReplicas
        else laggingLastPreparedN
    laggingLastPreparedN = if yesnono then cast myId else myLastPreparedN

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

    myPromisors :: Stream ()
    myPromisors = undefined

    validPromise, betterPromise :: Stream Bool
    validPromise = laggingStatus == constant trying && recvMsgType == constant promise && recvN == laggingLastPreparedN
    betterPromise = validPromise && recvLastN > laggingBestPromisedN

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

    myAckers :: Stream ()
    myAckers = undefined

    wannaPrepare, wannaPromise, wannaPoll, wannaAck :: Stream Bool
    wannaPromise = recvMsgType == constant prepare && recvN > laggingLastPromiseN
    wannaPoll = undefined
    wannaAck = recvMsgType == constant accept && recvN == laggingLastPromiseN
    wannaPrepare = not success && recvMsgType == constant noMsg && rand01 < 0.1

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
