module TBList where


import CASList
import AbstractLock
import STM
--import Control.Concurrent.STM
--import Control.Concurrent.STM.TVar
--import GHC.Conc


data ListHandle a = TBL ALock (CASList.ListHandle a)
--data Holder = Holder Int Bool

--instance Eq Holder where
--	(Holder key1 _) == (Holder key2 _) = key1 == key2


newList :: IO (TBList.ListHandle Int)
newList = do
	alock <- newALock
	list <- CASList.newList
	return (TBL alock list)


add:: TBList.ListHandle Int -> Int -> IO Bool
--addToTail :: TBIntList -> Int -> STM Bool
add (TBL alock list) key = atomically $ newTBSTM ac undo commit 
	where
	ac =  do
		ok<-lock alock key
		case ok of
			True -> do found <- CASList.find list key
				   if found then return (Just False)	
					    else do 
				      CASList.addToTail list key
				      return (Just True)
			False -> return Nothing
	undo v = do
		case v of
			Just True -> do
				CASList.delete list key
				AbstractLock.unlock alock key
				return()
			Just False ->  do
				AbstractLock.unlock alock key
				return()
			Nothing -> return ()
			
	commit = do 	AbstractLock.unlock alock key
			return ()
	
		
        
contains :: TBList.ListHandle Int -> Int -> IO Bool	
--find :: TBIntList -> Int -> STM Bool
contains (TBL alock list) key = atomically $ newTBSTM ac undo commit
	where
	ac = do
		ok<-lock alock key
		case ok of
			True -> do v <- CASList.find list key
				   return (Just v)
			False -> return Nothing
	undo v = do 	case v of
				Just _ -> do 
					AbstractLock.unlock alock key
					return ()
				Nothing -> return ()	
	commit = do 	AbstractLock.unlock alock key
			return ()

remove :: TBList.ListHandle Int -> Int -> IO Bool	
--delete :: TBIntList -> Int -> STM Bool
remove (TBL alock list) key = atomically $ newTBSTM ac undo commit
	where
	ac =  do 
		ok<-lock alock key
		case ok of
			True -> do v<-CASList.delete list key
				   return (Just v)
			False -> return Nothing

	undo ok = do
		case ok of
			Just True -> do
				CASList.addToTail list key				
				AbstractLock.unlock alock key
				return ()
			Just False-> do AbstractLock.unlock alock key
				        return ()
			Nothing -> return ()
	commit = do 	AbstractLock.unlock alock key
			return ()

--printList :: TBList.ListHandle Int -> IO ()
--printList (TBL alock list) = CASList.printList list	

--cntList :: TBList.ListHandle Int -> IO Int
--cntList (TBL alock list) = CASList.cntList list			
