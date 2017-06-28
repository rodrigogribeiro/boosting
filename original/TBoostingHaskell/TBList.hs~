module TBList where


import CASList
import AbstractLock
import STM
--import Control.Concurrent.STM
--import Control.Concurrent.STM.TVar
--import GHC.Conc

data TBIntList = TBL ALock (ListHandle Int)

--data Holder = Holder Int Bool

--instance Eq Holder where
--	(Holder key1 _) == (Holder key2 _) = key1 == key2



newTBList :: IO TBIntList
newTBList = do
	alock <- newALock
	list <- newList
	return (TBL alock list)


addToTail :: TBIntList -> Int -> STM ()
addToTail (TBL alock list) key = newTBSTM ac undo commit
	where
	ac =  do
		ok<-lock alock key
		case ok of
			True -> do CASList.addToTail list key
				   return (Just ())
			False -> return Nothing
	undo _ = do
		CASList.delete list key
		AbstractLock.unlock alock key
		return()
	commit = do 	AbstractLock.unlock alock key
			return ()
	
		
        
	
find :: TBIntList -> Int -> STM Bool
find (TBL alock list) key = newTBSTM ac undo commit
	where
	ac = do
		ok<-lock alock key
		case ok of
			True -> do v <- CASList.find list key
				   return (Just v)
			False -> return Nothing
	undo _ = return ()	
	commit = do 	AbstractLock.unlock alock key
			return ()
	
delete :: TBIntList -> Int -> STM Bool
delete (TBL alock list) key = newTBSTM ac undo commit
	where
	ac =  do 
		ok<-lock alock key
		case ok of
			True -> do v<-CASList.delete list key
				   return (Just v)
			False -> return Nothing

	undo ok = do
		case ok of
			True -> CASList.addToTail list key
			False -> return ()
	commit = do 	AbstractLock.unlock alock key
			return ()
		
