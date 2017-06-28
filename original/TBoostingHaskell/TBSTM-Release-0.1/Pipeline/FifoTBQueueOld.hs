module FifoTBQueue where

--import Control.Concurrent.STM
--import Control.Concurrent.STM.TChan
import Data.Concurrent.Deque.Reference
import STM

data TBQueue a = Q (SimpleDeque a) (TVar Int)

newFifo = do 	q<-newQ
		tvar <- newTVarIO 0
		return (Q q tvar)

writeFifo (Q c tvar) v = atomically (do size <- readTVar tvar
					writeTVar tvar (size+1)
					newTBSTM ac undo commit)
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


readFifo (Q c tvar) = atomically (do
		 size <- readTVar tvar
		 if size == 0 then retry else do writeTVar tvar (size-1)
						 newTBSTM ac undo commit)
	where
	ac = do
		mv <- tryPopR c
		case mv of
			Just v -> return (Just v)
			Nothing -> error ""
	undo v = pushR c v
	commit = return ()
		
