module FifoPQueue where

--import Control.Concurrent.STM
--import Control.Concurrent.STM.TChan
import Data.Concurrent.Deque.Reference
newFifo = newQ

writeFifo c v = pushL c v

readFifo c = do
	mv <- tryPopR c
	case mv of
		Just v -> return v
		Nothing -> readFifo c
