module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import FifoSTM

produce = do
	let n = fib 1
	seq n (return n)

consume v = do
	let n = fib 1
	seq n (return ())

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
 
main = do
	let n = 2500000
	mv <- newEmptyMVar
	f <- newFifo
	forkIO $ producer mv n f
	forkIO $ consumer mv n f
	takeMVar mv
	takeMVar mv

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
