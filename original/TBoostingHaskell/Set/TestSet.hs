module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import System.Time
import System.Environment
import System.Random
import Data.List
--import Control.Concurrent.STM
--import STMSet
import TBList

createSet :: ListHandle Int -> Int -> Int -> IO ()
createSet set range 0 = return ()
createSet set range n = do
	v<-randomRIO (1, range)
	ok <- add set v
	case ok of
		True -> createSet set range (n-1)
		False -> createSet set range n

main = do
	set <- newList
	args <- getArgs
	let range = read (args!!0)
	let size = read (args!!1)
	let ops = read (args !!2)	
	let nthreads = read (args !! 3)
	createSet set range size
	mvar <- newEmptyMVar
	putStrLn "Start"
     	timeStart <- getClockTime
	mapM (\ nops -> forkIO (thread set mvar range nops)) (replicate nthreads ops)
	mapM takeMVar (replicate nthreads mvar)
	timeEnd <- getClockTime	
	putStrLn "Done"
	let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
	print ("#"++show((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff))))))

thread :: ListHandle Int -> MVar () -> Int -> Int -> IO ()
thread set mvar range 0 = putMVar mvar ()
thread set mvar range n = do
	op <- randomRIO (1,5::Int)
	val <- randomRIO (1,range)
	case op of
		1 -> do
			_<-contains set val  
			thread set mvar range (n-1) 
		2 -> do
			_<- contains set val 
			thread set mvar range (n-1)
		3 -> do	
			_<-contains set val
			thread set mvar range (n-1)
		4 -> do	
			_<-remove set val
			thread set mvar range (n-1)
		5 -> do
			_<- add set val 
			thread set mvar range (n-1)



