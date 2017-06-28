
module TBList where


import CASList
import AbstractLock
import STM
--import Control.Concurrent.STM
--import Control.Concurrent.STM.TVar
--import GHC.Conc

--data TBIntList = TBL ALock (ListHandle Int)

--data Holder = Holder Int Bool

--instance Eq Holder where
--	(Holder key1 _) == (Holder key2 _) = key1 == key2

data ListHandle a = TBL ALock (CASList.ListHandle a)

newList :: IO (TBList.ListHandle Int)
newList = do
	alock <- newALock
	list <- CASList.newList
	return (TBL alock list)

addToTail :: TBList.ListHandle Int -> Int -> IO ()
-- addToTail :: TBList.ListH -> Int -> STM ()
addToTail (TBL alock list) key = atomically $ newTBSTM ac undo commit
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
	
		
        
	
-- find :: TBIntList -> Int -> STM Bool
find :: TBList.ListHandle Int -> Int -> IO Bool
find (TBL alock list) key = atomically $ newTBSTM ac undo commit
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

delete :: TBList.ListHandle Int -> Int -> IO Bool	
-- delete :: TBIntList -> Int -> STM Bool
delete (TBL alock list) key =  atomically $ newTBSTM ac undo commit
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


printList :: TBList.ListHandle Int -> IO ()
printList (TBL alock list) = CASList.printList list	

cntList :: TBList.ListHandle Int -> IO Int
cntList (TBL alock list) = CASList.cntList list	
