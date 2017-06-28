module TestSet where

import System.Random
import STMSet


createPrint set 0 = return ()
createPrint set n = do
	v <- randomRIO (1, 10)
	
