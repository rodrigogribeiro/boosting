module FifoTBQueue where

--import Control.Concurrent.STM
--import Control.Concurrent.STM.TChan
import Data.Concurrent.Deque.Reference
import STM

newFifo = newQ

writeFifo c v = atomically $ newTBSTM ac undo commit
	where
	ac = do
		pushL c v
		return (Just ())
	undo v = do
		mv <- tryPopL c
		case mv of
			Nothing -> error ""
			Just v -> return ()
 	commit = return ()


readFifo c = atomically $ newTBSTM ac undo commit
	where
	ac = do
		mv <- tryPopR c
		case mv of
			Just v -> return (Just v)
			Nothing -> ac
	undo v = pushR c v
	commit = return ()
		
