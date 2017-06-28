module TestSTMSet where

import System.Random
import STMSet
import Control.Concurrent.STM

createSet :: ListHandle Int -> Int -> IO ()
createSet set 0 = return ()
createSet set n = do
	v<-randomRIO (1, 20)
	atomically (add set v)
	printSet set
	createSet set (n-1)

main = do
	set <- newList
	createSet set 20
	containsSet set 20	


deleteSet :: ListHandle Int -> Int -> IO ()
deleteSet set 0 = return ()
deleteSet set n = do
	v<-randomRIO (1, 20)
	print v
	atomically (remove set v)
	printSet set
	deleteSet set (n-1)

containsSet :: ListHandle Int -> Int -> IO ()
containsSet set 0 = return ()
containsSet set n = do
	v<-randomRIO (1, 20)
	print v
	ok<- atomically (contains set v)
	print ok
	printSet set
	containsSet set (n-1)
