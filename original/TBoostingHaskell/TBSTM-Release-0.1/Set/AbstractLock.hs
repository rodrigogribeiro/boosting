module AbstractLock where

import Control.Concurrent(myThreadId)
--import Control.Monad.STM
import Data.THash
import Data.IORef
import STM


data ALock = ALock (THash Int Lock)

data Lock = Lock (IORef Integer) (IORef Integer)

mytid2 :: IO Integer
mytid2 = do
	x <- myThreadId
	return ((read . drop 9) (show x))

--atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
--atomCAS ptr old new =
 --  atomicModifyIORef ptr (\ cur -> if cur == old
 --                                  then (new, True)
   --                                else (cur, False))

newALock :: IO ALock
newALock = do
	hasht <- atomically (new hashInt)
	return (ALock hasht)

plusOne :: IORef Integer -> IO ()
plusOne ref = do
	v <-readIORef ref
	writeIORef ref (v+1)

decCounter:: IORef Integer -> IO ()
decCounter ref = do
	v <-readIORef ref
	writeIORef ref (v-1)

lock :: ALock -> Int -> IO Bool
lock alock@(ALock ht) key = do
	mior <- atomically (Data.THash.lookup ht key)
	myId <- mytid2
	case mior of
		Just (Lock ior counter) -> do
			v <- readIORef ior
			if (v == 0) 
				then do locked<- atomCAS ior 0 myId
					case locked of
						True -> do
							plusOne counter
							return True
						False -> return False
	
				else if (v==myId) 	then do
								plusOne counter
								return True
							else return False	

		Nothing -> do
			ior <- newIORef myId
	                counter <- newIORef 1
			ok <- atomically (insert ht key (Lock ior counter))
			if ok then return True else (lock alock key)

unlock :: ALock -> Int -> IO Bool
unlock alock@(ALock ht) key = do
	mior <- atomically (Data.THash.lookup ht key)
	myId <- mytid2
	case mior of
		Just (Lock ior counter) -> do
			nlocks <- readIORef counter
			if nlocks>1 	then do decCounter counter
						return True
					else do
						decCounter counter
						ok <- atomCAS ior myId 0
						if ok then return () else error "not ok unlock"
						return ok 
		Nothing -> error "Nothing unlock"

