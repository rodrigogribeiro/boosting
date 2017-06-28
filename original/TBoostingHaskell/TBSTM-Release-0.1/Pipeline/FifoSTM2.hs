module FifoSTM2 where

import STM
import TChan

newFifo = newTChanIO

writeFifo c v = atomically $ writeTChan c v

readFifo c = atomically $ readTChan c
