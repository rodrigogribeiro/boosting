module UniqueIDCAS where

import Data.IORef

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORefCAS ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

type IDGer = IORef Int

newID :: IO IDGer
newID = newIORef 0

getID ::  IDGer -> IO Int
getID idger = do
	v <- readIORef idger
	ok <- atomCAS idger v (v+1)
	if ok then return (v+1) else getID idger





