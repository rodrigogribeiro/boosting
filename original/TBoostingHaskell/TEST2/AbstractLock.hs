module AbstractLock where

import Control.Concurrent
import Control.Monad.STM
import Data.THash
import Data.IORef



data ALock = ALock (THash Int (IORef Integer))

myTId :: IO Integer
myTId = do
	x <- myThreadId
	return ((read . drop 9) (show x))

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORef ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

newALock :: IO ALock
newALock = do
	hasht <- atomically (new hashInt)
	return (ALock hasht)


lock :: ALock -> Int -> IO Bool
lock alock@(ALock ht) key = do
	mior <- atomically (Data.THash.lookup ht key)
	myId <- myTId
	case mior of
		Just ior -> do
			v <- readIORef ior
			if (v == 0) 
				then atomCAS ior 0 myId
				else return (v==myId)
		Nothing -> do
			ior <- newIORef myId
			ok <- atomically (insert ht key ior)
			if ok then return True else (lock alock key)

unlock :: ALock -> Int -> IO Bool
unlock alock@(ALock ht) key = do
	mior <- atomically (Data.THash.lookup ht key)
	myId <- myTId
	case mior of
		Just ior -> do
			ok <- atomCAS ior myId 0
			if ok then return () else error "not ok unlock"
			return ok 
		Nothing -> error "Nothing unlock"

