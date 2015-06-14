{-# LANGUAGE RebindableSyntax #-}

module Paxos (paxos) where

import Language.Copilot
import qualified Prelude as P

noMsg, prepare, promiseAnything, promiseLast, accept, acknowledge :: Word8
noMsg : prepare : promiseAnything : promiseLast : accept : acknowledge : _ = [0..]

quorum :: Int
quorum = 3

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
    recvN       = externW32 "recv_n" Nothing
    recvLastN   = externD "recv_last_n" Nothing
    recvTemp    = externD "recv_temp" Nothing

    -- for now
    sendMsgType = constW8 noMsg
    sendN = constW32 0
    sendLastN = constW32 0
    sendTemp = constD 0
    success = constB True
    successTemp = temp
    successHum = successTemp

    {-
    -- pseudocode

    -- state
    myLastPreparedN := Nothing
    myLastAckN := Nothing
    myLastAckTemp := Nothing
    myLastPromiseN := Nothing
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
    sendLastN := whatever
    sendTemp := whatever
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
