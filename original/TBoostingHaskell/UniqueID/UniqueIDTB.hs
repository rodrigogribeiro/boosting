module UniqueIDTB where

import Data.IORef
import UniqueIDCAS
import STM

type IDGer = IORef Int

newID :: IO UniqueIDCAS.IDGer
newID = UniqueIDCAS.newID

getID ::  UniqueIDCAS.IDGer -> IO Int
getID idger = atomically $ newTBSTM ac undo commit
	where
	ac = do
		id <- UniqueIDCAS.getID idger
		return (Just id)
	undo _ = return ()
	commit = return ()
	
