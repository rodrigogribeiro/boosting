module STM where

--import GHC.Conc(myTId)
import Data.IORef
import Control.Concurrent
--import Data.HashTable
import Control.Concurrent.MVar
import System.IO.Unsafe
--import Data.List
--import Foreign.StablePtr
--import System.Mem.StableName
--import Foreign.Ptr
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map


assert :: Bool -> String -> IO ()
assert False s = error s
assert True s  = return ()


assert2 :: Eq a => a -> a -> String -> a
assert2 v1 v2 str = if v2 == v1 then v1 else error str

data STM a = STM (TState -> IO (TResult a))

data TResult a = Valid TState a | Retry TState | Invalid TState

instance Monad STM where
	(STM t1) >>= f = STM(\tState -> do
				tRes <- t1 tState				
				case tRes of
					Valid nTState v ->
						let (STM t2) = f v in 
							t2 nTState
					Retry nTState 	-> return (Retry nTState)
					Invalid  nTState-> return (Invalid nTState)
				)

	return x = STM (\tState -> return (Valid tState x))




data TState = Tr {
   transId :: TId,
   readStamp :: IORef Integer,
   readSet :: ReadSet,
   writeSet :: WriteSet,
   commitAction :: IO (),
   undo :: IO (),
   greedy :: IORef Integer,
   tbUndo :: IO (),
   tbCommit :: IO ()
}

instance Eq TState where
	(Tr id1 _ _ _ _ _ _ _ _) == (Tr id2 _ _ _ _ _ _ _ _) = id1 == id2


retry :: STM a
retry = STM $ \tState -> return (Retry tState)

---- ==============================================

orElse :: STM a -> STM a -> STM a
orElse (STM t1) (STM t2) = STM $ \tstate -> do
	--tsCopy <- cloneTState tstate
	tRes1 <- t1 tstate{undo= return ()}
	case tRes1 of
		Retry nTState1 	-> do
				undo nTState1
				tRes2 <- t2 tstate
				case tRes2 of
					Retry nTState2 -> do	fTState <- mergeTStates nTState2 nTState1 
								return (Retry fTState)
					Valid nTState2 r ->  do	fTState <- mergeTStates nTState2 nTState1
								return (Valid fTState r)
					_ ->         return tRes2
		_	-> return tRes1

--cloneTState :: TState -> IO TState
--cloneTState tstate = return (Tr (transId tstate) (readStamp tstate) (readSet tstate) (writeSet tstate) (commitAction tstate))

unreadTVar :: TVar a -> STM ()
unreadTVar tvar@(TVar rlock wlock id content ncontent queue) = STM $ \tstate -> do
	let found = lookUpWS (writeSet tstate) id
	case found of
		True -> return (Valid tstate ())
		False -> do  let newRs =deleteRS (readSet tstate) id
			     return (Valid tstate{readSet=newRs} ())

lookUpWS :: WriteSet -> Integer -> Bool
lookUpWS [] id = False
lookUpWS ((_,_,tvarid):xs) id
	| tvarid == id = True
	| otherwise = lookUpWS xs id


mergeTStates :: TState -> TState -> IO TState
mergeTStates ts1 ts2 = do
	let nrs = Set.union (readSet ts1) (readSet ts2)
	--writeIORef (readSet ts1) nrs
	return (ts1{readSet=nrs})
-------------------------------------------      END ORELSE          -------------

newTState :: IO TState
newTState = do
	tid <- myTId
	--assert (even tid) "Transaction id is not even!"
	rs <- readIORef globalClock --incrementGlobalClock
	readStamp <- newIORef rs
	--assert (odd readStamp) "newtstate: globalclock not odd!"
	--readSet <- newIORef (Set.empty)
	--writeSet <- newIORef (Map.empty)
	--writeSet <- new (==) (\x -> hashInt (fromInteger x))
	greedy <- newIORef 0
        return (Tr tid readStamp newRS [] (return()) (return ()) greedy (return ()) (return()) )


type TId = Integer


myTId :: IO TId
myTId = do
	x <- myThreadId
	return (((2*). read . drop 9) (show x))



data TVar a = TVar{
	rlock :: RLock,
	wlock :: WLock,
	id :: Integer,
--	writeStamp :: IORef Integer,
	content :: IORef a,
	ncontent :: IORef a,
        waitQueue :: IORef [MVar ()]
}


-- wlock Locked= trans id
-- rlock unlocked= version
-- tvar@(TVar rlock wlock id content ncontent queue)

newTVarIO :: a -> IO (TVar a)
newTVarIO a = do
		rlock <- newRLock
		wlock <- newWLock
		id <- newID
		content <- newIORef a
		ncontent <- newIORef a
		waitQueue <- newIORef []
		return (TVar rlock wlock id content ncontent waitQueue)


instance Eq (TVar a) where
	(TVar l1 _ _ _ _ _) == (TVar l2 _ _ _ _ _) = l1 == l2



--newID2 :: Lock -> IO Integer
--newID2 lock = do stn <- makeStableName lock
--	        return (hashStableName stn)


idref :: IORef Integer
idref = unsafePerformIO (newIORef 0)

newID :: IO Integer
newID = do
	--print "increment"
	cur <- readIORef idref
	changed <- atomCAS idref cur (cur+1)
	if changed then return (cur+1) else newID
	

newTVar :: a -> STM (TVar a)
newTVar a = STM $ \tState -> do
			tvar <- newTVarIO a		
			return (Valid tState tvar)



-- wlock Locked= trans id
-- rlock unlocked= version

recycleTS :: TState -> IO TState
recycleTS (Tr tid readstamp rs ws ac un greedy tbu tbc) = do
	--tid <- myTId
	--assert (even tid) "Transaction id is not even!"
	clock <- readIORef globalClock --incrementGlobalClock
	nreadStamp <- newIORef clock
	--assert (odd readStamp) "newtstate: globalclock not odd!"
	--readSet <- newIORef (Set.empty)
	--writeSet <- newIORef (Map.empty)
	--writeSet <- new (==) (\x -> hashInt (fromInteger x))
	greedy <- newIORef 0
        return (Tr tid nreadStamp newRS [] (return()) (return ()) greedy (return()) (return()))


atomically :: STM a -> IO a
atomically stmac = do
--	print "ATOMICALY"
	ts <- newTState
	atomically2 ts stmac

atomically2 :: TState -> STM a -> IO a
atomically2 ts stmac@(STM ac) = do
	r <- ac ts
	--print "Executou transacao"
	case r of
		Invalid nts ->	do	--print "Invalid" 
					tbUndo nts		
					rollBack nts
					recicled <- recycleTS nts
				   	atomically2 recicled stmac
		Retry nts   -> do -- validateAndAcquireWLocks tid readStamp wset
				readStmp <- readIORef (readStamp nts)
				let readSt = Set.toList (readSet nts)
				(ok,locks) <- validateAndAcquireWLocks nts (transId nts) readStmp readSt
				-- ok <- validateTS nts
				if (not ok) 	
						then do
							unlock nts locks
							rollBack nts
							recicled <- recycleTS nts
				   			atomically2 recicled stmac
				   			--atomically stmac
						else do
						waitMVar <- newEmptyMVar
						addToWaitQueues waitMVar readSt
						--clean nts
						unlock nts locks
						rollBack nts
						takeMVar waitMVar
						recicled <- recycleTS nts
				   		atomically2 recicled stmac
						--atomically stmac
		Valid nts a ->  do
				--print "Valid"
				if isEmptyWS (writeSet nts) 	
					then tbCommit nts >> return a 
					else do
				--print "rlocks locked"
				ti <- myTId 
				versions <- lockRLocks ti (writeSet nts) -- DO WE NEED ATOMCAS? No because we read the content twice at readTVar
				wstamp <- incrementGlobalClock				-- ALL WRITE LOCKS ARE LOCKED, WILL I EVER
				readStamp <- readIORef (readStamp nts) 	-- LOCK RLOCK IF I DONT HAVE WRITELOCK? NO, BUT I WILL READ ITS CONTENT
				--print (show wstamp)
				--print (show readStamp)
				if (wstamp > (readStamp +2) )
					then do let readSt = Set.toList (readSet nts)
						ok <- validateTS ti readSt
						if (not ok) 	then do	-- rollBack nts
									tbUndo nts
									unlockWriteSet nts (writeSet nts) (reverse versions) ti
						 			-- mapM_ (\(wlock,rlock,_)-> atomCAS rlock wstamp 0  >> atomCAS wlock ti 0) (writeSet nts)							
									--print "Nao deu!!!"
									recicled <- recycleTS nts
				   					atomically2 recicled stmac
									--atomically stmac
								else do tbCommit nts
  									commitAction nts
									-- ti <- myTId
									mapM_ (\(rlock,wlock,_)-> atomCAS rlock ti wstamp  >> atomCAS wlock (Just nts) Nothing) (writeSet nts)
									--print "commit1"		
									--print (writeSet nts)							
									return a
					else do 
					tbCommit nts
			 		commitAction nts
					-- ti <- myTId
					mapM (\(rlock,wlock,_)-> atomCAS rlock ti wstamp >> atomCAS wlock (Just nts) Nothing) (writeSet nts)
					--print "commit2"
					return a


--------------------------------------- CONTENTION MANAGER


cmOnRollBack tstate = return ()

cmOnWritetx tstate = return ()

shouldAbort wlock tid = return True




--cmOnWritetx tstate = do
--		g <-readIORef (greedy tstate) 
--		if (g == 0 && ((length (writeSet tstate)) == timeToGreedy))
--			then do 
--				ng <- incrementAndGetCM
--				writeIORef (greedy tstate) ng
--			else return ()

--shouldAbort tstate wlock tid = do
--		g <-readIORef (greedy tstate)
--		if g == 0 
--			then return True
--			else do
				
			



greedyts :: IORef Integer
greedyts = unsafePerformIO (newIORef 1)


globalTable :: MVar (Map.Map Integer (IORef Integer))
globalTable = unsafePerformIO (newMVar Map.empty)

incrementAndGetCM :: IO Integer
incrementAndGetCM = do
	ov <- readIORef greedyts
	changed <- atomCAS greedyts ov (ov+1)
	if changed then do --assert (odd (ov+2)) "Clock is not an odd number!"
			   return (ov+1) 
		   else incrementAndGetCM

timeToGreedy = 10
	


-------------------------------------------------


addToWaitQueues :: MVar () ->  [RSEntry]  -> IO ()
addToWaitQueues mvar  = mapM_ (\(RSE _ _ _ _ queue) -> do
					--assert (isLockedIO lock) ("AddtoQueues: tvar not locked!!!")
					list <- readIORef queue
					writeIORef queue (mvar:list)) 


unlock :: TState ->[WLock] -> IO ()
unlock tid = mapM_ (\lock ->  do 	
					unlocked<- atomCAS lock (Just tid) Nothing
					assert unlocked "COULD NOT UNLOCK LOCK"
					return ())

--(ok,locks) <- validateAndAcquireWLocks (transId nts) (readStamp nts) (writeSet nts)

validateAndAcquireWLocks :: TState -> Integer -> Integer -> [RSEntry]  -> IO (Bool,[WLock])
validateAndAcquireWLocks tstate tid readStamp rset = validateAWL tstate tid readStamp rset []

validateAWL :: TState -> Integer -> Integer -> [RSEntry]  -> [WLock] -> IO (Bool,[WLock])
validateAWL tstate tid readStamp [] locks = return (True,locks)
validateAWL tstate tid readStamp ((RSE _ rlock wlock _ _):rset) locks = 
		do
		stmp <- readIORef rlock
		if (readStamp <= stmp) 	then do 
						lockValue <- readIORef wlock
						if (isLockedByV lockValue tid)
							then validateAWL tstate tid readStamp rset locks 
							else do
								ok <- lockForWriting wlock tstate
                        	        			case ok of
									True -> validateAWL tstate tid readStamp rset (wlock:locks)
									False -> return (False, locks)
					else return (False,locks)
	



lockRLocks :: Integer -> WriteSet -> IO [Integer]
lockRLocks tid ws = mapM (\(rlock,_,_) -> lockRLock rlock) ws
	where lockRLock rlock = do version <- readIORef rlock 
				   atomCAS rlock version tid 
				   return version 

unlockWriteSet :: TState -> WriteSet -> [Integer] -> Integer -> IO ()
unlockWriteSet tstate [] [] ti = return ()
unlockWriteSet tstate ((rlock,wlock,_):xs) (ver:ys) ti = do  
			atomCAS rlock ti ver -- WHAT HAPPENS IF RLOCK WAS UNLOCKED/LOCKED AND IT IS IN BOTH LISTS
			atomCAS wlock (Just tstate) Nothing
			unlockWriteSet tstate xs ys ti



-----------------------					




rollBack :: TState -> IO ()
rollBack tstate = do
--	ti <- myTId
	mapM_ (\(_,wlock,_)-> atomCAS wlock (Just tstate) Nothing >>= \v -> return (assert2 v True "rollback" `seq` v)) (writeSet tstate)
	cmOnRollBack tstate



--validateTS :: TState -> IO Bool
--validateTS ts = validate_ (transId ts) (readSet ts)
--	where
--	validate_ :: Integer -> ReadSet -> IO Bool
--	validate_ transId [] = return True
--	validate_ transId ((rlock,_,version,_):rs) = do
--				cv <- readIORef rlock
--				-- locked <- isLockedRLIO rlock
--				if (cv /= version && not (cv == transId)) then return False  else validate_ transId rs


validateTS ::  Integer -> [RSEntry]  -> IO Bool
validateTS transId [] = return True
validateTS transId ((RSE _ rlock _ version _):rs) = do
				cv <- readIORef rlock
				-- locked <- isLockedRLIO rlock
				if (cv /= version && not (cv == transId)) then return False  else validateTS transId rs








extend :: TState -> IO Bool
extend ts = do
	c <- readIORef globalClock
	valid <- validateTS (transId ts) (Set.toList (readSet ts))
	if valid
		then do
			writeIORef (readStamp ts) c
			return True
		else return False

--validateTS tstate = do map (\rlock,version -> readIORef rlock >>= \cv -> return (cv == version)) (readSet tstate)







------------LOCKS 

-- wlock Locked= trans id
-- rlock unlocked= version


-- wlock => Locked = thid (even), unlocked = 0
-- rlock => Locked = thid (even), unlocked= version (odd)


type RLock = IORef Integer

type WLock = IORef (Maybe TState)


newRLock :: IO RLock
newRLock = newIORef 1


newWLock :: IO WLock
newWLock = newIORef Nothing


 
isLockedRLIO :: RLock -> IO Bool
isLockedRLIO rlock = do v <- readIORef rlock
		        return (even v)

isLockedRL :: Integer -> Bool
isLockedRL v = even v 



--unlock :: Integer -> [(IORef Integer,WLock)] -> IO ()
--unlock tid = mapM_ (\(iows,lock) -> do 	ws <- readIORef iows
--					unlocked<- atomCAS lock tid ws
--					--return ()
--					--assert unlocked "COULD NOT UNLOCK LOCK"
--					return ())
--			           	


isLockedWL :: Maybe TState -> Bool
isLockedWL v = not (v==Nothing)

isLockedWLIO :: WLock -> Bool
isLockedWLIO lock = unsafePerformIO $ do
					v<- readIORef lock
					return (isLockedWL v)

isLockedBy :: WLock -> Integer-> IO Bool
isLockedBy wlock tid = do 	v <- readIORef wlock
				case v of
					Nothing -> return False
					Just (Tr id _ _ _ _ _ _ _ _) -> return (id==tid)

isLockedByV :: Maybe TState -> Integer ->Bool 
isLockedByV v tid = case v of
			Nothing -> False
			Just (Tr id _ _ _ _ _ _ _ _) -> id==tid

readWLock :: WLock -> IO (Maybe TState)
readWLock = readIORef

readRLock :: RLock -> IO Integer
readRLock = readIORef

--------------- GLOBAL CLOCK

globalClock :: IORef Integer
globalClock = unsafePerformIO (newIORef 1)

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new = atomicModifyIORef ptr (\cur -> if cur == old then (new, True) else (cur,False))

incrementGlobalClock :: IO Integer
incrementGlobalClock = do
	ov <- readIORef globalClock
	changed <- atomCAS globalClock ov (ov+2)
	if changed then do --assert (odd (ov+2)) "Clock is not an odd number!"
			   return (ov+2) 
		   else incrementGlobalClock
	

----------------------------------------------
----------- Read and Write Sets -----------------

-- type ReadSet = [RSEntry]   --IORef [(Lock, IORef Int)]

type ReadSet = Set RSEntry

data RSEntry = RSE Integer RLock WLock Integer (IORef [MVar ()])  -- RSE Integer RLock (IORef Integer) (IORef [MVar ()])


instance Eq RSEntry where
	(RSE id1 _ _ _ _) == (RSE id2 _ _ _ _) = id1 == id2

instance Ord RSEntry where
	compare (RSE id1 _ _ _ _) (RSE id2 _ _ _ _)
		| id1 == id2    	=  EQ
         	| id1 <= id2    	=  LT
         	| otherwise 		=  GT

putRS :: ReadSet -> RSEntry -> ReadSet
putRS rs en = Set.insert en rs

newRS :: ReadSet
newRS = Set.empty

deleteRS :: ReadSet -> Integer -> ReadSet
deleteRS rs id = Set.delete (RSE id dummyrlock dummywlock 0 dummyqueue) rs


dummyrlock :: RLock
dummyrlock = unsafePerformIO (newIORef 1)

dummywlock :: WLock
dummywlock = unsafePerformIO (newIORef Nothing)

dummyqueue ::IORef [MVar ()]
dummyqueue = unsafePerformIO ( newIORef [])

--putRS :: ReadSet -> RSEntry -> ReadSet
-- putRS rs en = en : rs

-- newRS :: ReadSet
-- newRS = []


type WriteSet = [WSEntry] -- IORef (Map.Map Integer (Ptr()))

type WSEntry =  (RLock,WLock,Integer) --WSE WLock (IORef Integer)  (IORef [MVar ()]) -- baguncei o tipo



-- WSENTRY = (RLOCK, WLOCK, TRANS ID)
putWS :: WriteSet -> WSEntry -> WriteSet
putWS ws en = en: ws

isEmptyWS ws = ws == []



------

-- wlock Locked= trans id
-- rlock unlocked= version


writeTVar :: TVar a -> a -> STM ()
writeTVar tvar@(TVar rlock wlock id content ncontent queue) newValue = STM $ \tState -> do
	lockedByme <- isLockedBy wlock (transId tState)
	if lockedByme
		then do 
			writeIORef ncontent newValue
        		return (Valid tState ()) 
		else do
		 ok <- lockForWriting wlock tState
		 case ok of 
		  False -> 	--do
				--rollBack tState
			return (Invalid tState)			
		  True -> do
			writeIORef ncontent newValue
			acontent <- readIORef content
			let nws = putWS (writeSet tState) (rlock,wlock,id)  ---- ?????????????????
			version <- readIORef rlock -- it is safe as i can only modify rlock if i already have wlock
			rsp <- readIORef (readStamp tState)
			if (version > rsp)
				then  do 
					ok <- extend tState -- is state visible outside the call??? 
					if not ok then  do 	
						--rollBack tState{writeSet=nws}
						return (Invalid tState{writeSet=nws}) ---- ?????
						else do
						cmOnWritetx tState
						let newCommitAction = do	
							commitAction tState
							nc <- readIORef ncontent
							writeIORef content nc
							listMVars <- readIORef queue
							mapM_ (\mvar -> tryPutMVar mvar ()) listMVars
							writeIORef queue []
						    undo2 = do
							undo tState
							writeIORef content acontent
							writeIORef wlock Nothing
						return (Valid tState{writeSet=nws, commitAction = newCommitAction, undo = undo2} ())
                         else do 			
				cmOnWritetx tState
				let newCommitAction = do	
					commitAction tState
					nc <- readIORef ncontent
					writeIORef content nc
					listMVars <- readIORef queue
					mapM_ (\mvar -> tryPutMVar mvar ()) listMVars
					writeIORef queue []
				return (Valid tState{writeSet=nws, commitAction = newCommitAction} ())
                        
			



lockForWriting wlock tstate =
		do 
		lockValue <- readIORef wlock  	
		if (isLockedWL lockValue) 
		 then do
				abort<- shouldAbort wlock tstate 
				if abort then return False  else lockForWriting wlock tstate -- if it does not abort, what happens? who aborts?
		 else do									-- is it not going to keep aborting?
			ok<- atomCAS wlock Nothing (Just tstate)
			case ok of
				True -> return True
				False -> lockForWriting wlock tstate
				
		


-- 

readTVar :: TVar a -> STM a
readTVar tvar@(TVar rlock wlock id content ncontent queue) = STM $ \tState -> do
	lockedByMe <- isLockedBy wlock (transId tState)	
	if lockedByMe
	 then do 	value <-readIORef ncontent
			return (Valid tState value)
	 else do
		version1 <- readIORef rlock
		(value,version) <- readValue version1 rlock content
         	let nrs = putRS (readSet tState) (RSE id rlock wlock version queue)--(RSE id rlock queue version) ---- ???????
		rsp <- readIORef (readStamp tState)
		if (version > rsp)
			then do
				ok <- extend tState{readSet=nrs}
				if not ok
					then do --rollBack tState{readSet=nrs} --- roll back should be done at atomic.
						return (Invalid tState{readSet=nrs}) -- return (Valid tState value)-- ---- ?????
					else return (Valid tState{readSet=nrs} value)
			else 	return (Valid tState{readSet=nrs} value)


    
readValue :: Integer -> RLock -> IORef a -> IO (a,Integer)
readValue lockValue rlock content = 	
					if isLockedRL lockValue 
				   	then do
						lv <- readIORef rlock
	                                	readValue lv rlock content
					else do
						value <- readIORef content
						lockValue2 <- readIORef rlock
						if lockValue == lockValue2 
						 then return (value,lockValue)
						 else readValue lockValue2 rlock content


newTBSTM :: IO (Maybe a) -> (a-> IO ()) -> IO () -> STM a
newTBSTM mac undo commit = STM $ \tState -> do
	r <- mac
	case r of
		Just v -> return (Valid tState{tbUndo = (tbUndo tState) >> undo v, tbCommit = (tbCommit tState) >> commit}  v)
		Nothing -> return (Invalid tState)

