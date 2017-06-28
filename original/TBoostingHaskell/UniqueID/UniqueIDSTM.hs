module UniqueIDSTM where

--import Control.Concurrent
--import Control.Monad.STM
import Control.Concurrent.STM

type IDGer = TVar Int

newID :: IO IDGer
newID = atomically $ do 
	id <- newTVar 0	
	return id


getID :: IDGer -> IO Int
getID tvar = atomically $ ac tvar
	where
	ac tvar= do
		v <- readTVar tvar
		writeTVar tvar (v+1)
		return v


