module AbstractLock where

import Control.Concurrent
import Control.Monad.STM
import Data.THash
import Data.IORef



data ALock = ALock (THash Int (IORef Integer))

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
	case mior of
		Just ior -> do
			ok <- atomCAS ior 0 1
			if ok then return True else return False
		Nothing -> do
			ior <- newIORef 1
			ok <- atomically (insert ht key ior)
			if ok then return True else (lock alock key)

unlock :: ALock -> Int -> IO Bool
unlock alock@(ALock ht) key = do
	mior <- atomically (Data.THash.lookup ht key)
	case mior of
		Just ior -> do
			ok <- atomCAS ior 1 0
			if ok then return True else return False

