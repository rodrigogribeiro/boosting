 module Main where

 import Control.Concurrent
-- import STM -- inserida para trabalhar com a nova implementação -- import GHC.Conc
 import Control.Monad
 import System.Time
 import Text.Printf
 import System.Environment
 --import UniqueIDSTM
 import UniqueIDCAS
 --import UniqueIDTB

 createThread :: Int -> IDGer -> MVar Int -> IO ThreadId
 createThread numOps tValue mvar =
 	 forkIO ( do
			{
			callNTimes numOps 
				(do 
				{ v <-getID tValue;
				  return ()
			      	}) 
			; putMVar mvar 1
			}
          	 )

 createThreads :: Int -> Int -> IDGer -> [MVar Int] -> IO()
 createThreads n numOps tVar mvars
	= mapM_ (createThread numOps tVar) mvars

	

 main1 :: Int -> Int -> IO () 
 main1 numops numThreads = do
	{ theSharedInt <- newID

	; timeStart <- getClockTime

	; mvars <- replicateM numThreads newEmptyMVar
	; print(numops)

	; threads <- createThreads numThreads numops theSharedInt mvars

	; mapM_ takeMVar mvars

	; timeEnd <- getClockTime
	; theSharedIntValue <- getID theSharedInt 
	; putStrLn (show (theSharedIntValue))

	; let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
	
	--; readTStats
		
	--; print ((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff)))))  -- linha original do programa
	;print ("#"++show((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff))))))

	; return ()
	}

 callNTimes :: Int -> IO () -> IO ()
 callNTimes 0 _ = return ()
 callNTimes times f = do{f 
			; callNTimes (times - 1) f
			}

 
 main :: IO()
 main =    do { args <- getArgs
		; let numops = read (args!!0)
		; let numthreads = read (args!!1)
		; let numiterations = read (args!!2)
		; callNTimes numiterations (main1 numops numthreads)
	      }


