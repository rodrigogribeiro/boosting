module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import System.Time
import System.Environment
import FifoTBQueue
--import FifoSTM
--import FifoPQueue
produce = do
	let n = fib 2
	seq n (return n)

consume v = do
	let n = fib 2
	seq n (return ())

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
 
putvalues q 0 = return ()
putvalues q n = writeFifo q 1 >> putvalues q (n-1)

main = do
	args <- getArgs
	let n = read (args!!0)
	--let n = 1000000 --1000000
	mv <- newEmptyMVar
	f <- newFifo
	timeStart <- getClockTime
	forkIO $ producer mv n f
	forkIO $ consumer mv n f
	takeMVar mv
	takeMVar mv
	timeEnd <- getClockTime
	let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
	print ("#"++show((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff))))))


producer mv 0 q = putMVar mv 1
producer mv n q = do
	v <- produce
	writeFifo q v
	producer mv (n-1) q

consumer mv 0 q = putMVar mv 1
consumer mv n q = do
	v <- readFifo q
	consume v
	consumer mv (n-1) q 
