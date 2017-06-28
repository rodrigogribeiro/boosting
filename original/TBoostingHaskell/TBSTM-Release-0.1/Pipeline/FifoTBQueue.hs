module FifoTBQueue where

--import Control.Concurrent.STM
--import Control.Concurrent.STM.TChan
import Data.Concurrent.Deque.Reference
import STM
import Data.CAS
import Data.IORef
--atomCASr :: Eq a => IORef a -> a -> a -> IO Bool
--atomCASr ptr old new =
 --  atomicModifyIORefCAS ptr (\ cur -> if cur == old
  --                                 then (new, True)
    --                               else (cur, False))


data TBQueue a = Q (SimpleDeque a) (IORef Int)

newFifo = do 	q<-newQ
		ioref <- newIORef 0
		return (Q q ioref)

writeFifo (Q c ioref) v = atomically (newTBSTM ac undo commit)
	where
	ac = do
		pushL c v
		return (Just ())
	undo v = do
		mv <- tryPopL c
		case mv of
			Nothing -> error ""
			Just v -> return ()
 	commit = do
		v <- readIORef ioref		
		ok<- atomCAS ioref v (v+1)
		if ok then return () else commit


readFifo (Q c ioref) = atomically (newTBSTM ac undo commit)
	where
	ac = do
		size<-readIORef ioref
		if size ==0 then return Nothing
			    else do
				--tryPopR c --- do	
				decIORef ioref
 				tryPopR c
		--	case mv of
		--		Just v -> return (Just v)
		--		Nothing -> error ""
	undo v = case v of
			Nothing -> return ()
			Just x -> incIORef ioref >> pushR c x
	commit = return ()

decIORef ioref = do	v <- readIORef ioref		
			ok<- atomCAS ioref v (v-1)
			if ok then return () else decIORef ioref	

incIORef ioref = do 	v <- readIORef ioref		
			ok<- atomCAS ioref v (v+1)
			if ok then return () else incIORef ioref
