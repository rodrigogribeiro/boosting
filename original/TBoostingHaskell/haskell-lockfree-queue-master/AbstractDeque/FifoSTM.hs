module FifoSTM where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

newFifo = newTChanIO

writeFifo c v = atomically $ writeTChan c v

readFifo c = atomically $ readTChan c
