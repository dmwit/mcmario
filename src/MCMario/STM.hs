-- | Address two shortcomings of STM: pattern match failures throw an exception
-- instead of blocking until they match, and atomically isn't MonadIO-aware.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module MCMario.STM
	( STM
	, STM.TMVar
	, STM.TVar
	, atomically
	, retry
	, newTVar
	, newTVarIO
	, readTVar
	, readTVarIO
	, writeTVar
	, modifyTVar
	, modifyTVar'
	, swapTVar
	, newTMVar
	, newEmptyTMVar
	, newTMVarIO
	, newEmptyTMVarIO
	, takeTMVar
	, putTMVar
	, readTMVar
	, tryReadTMVar
	, swapTMVar
	, tryTakeTMVar
	, tryPutTMVar
	, isEmptyTMVar
	) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Coerce
import qualified Control.Concurrent.STM as STM

newtype STM a = STM { realSTM :: STM.STM a } deriving (Functor, Applicative, Alternative)

instance Monad STM where
	return = STM . return
	STM m >>= f = STM (m >>= realSTM . f)
	STM m >> STM m' = STM (m >> m')
	fail _ = retry

zero :: STM.STM a -> STM a
one :: (a -> STM.STM b) -> (a -> STM b)
two :: (a -> b -> STM.STM c) -> (a -> b -> STM c)
zero = coerce
one = coerce
two = coerce

atomically = liftIO . STM.atomically . realSTM
retry = zero STM.retry
newTVar = one STM.newTVar
newTVarIO = liftIO . STM.newTVarIO
readTVar = one STM.readTVar
readTVarIO = liftIO . STM.readTVarIO
writeTVar = two STM.writeTVar
modifyTVar = two STM.modifyTVar
modifyTVar' = two STM.modifyTVar'
swapTVar = two STM.swapTVar
newTMVar = one STM.newTMVar
newEmptyTMVar = zero STM.newEmptyTMVar
newTMVarIO = liftIO . STM.newTMVarIO
newEmptyTMVarIO = liftIO STM.newEmptyTMVarIO
takeTMVar = one STM.takeTMVar
putTMVar = two STM.putTMVar
readTMVar = one STM.readTMVar
tryReadTMVar = one STM.tryReadTMVar
swapTMVar = two STM.swapTMVar
tryTakeTMVar = one STM.tryTakeTMVar
tryPutTMVar = two STM.tryPutTMVar
isEmptyTMVar = one STM.isEmptyTMVar
