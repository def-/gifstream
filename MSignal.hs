-- | A Module for broadcast signalling between threads.

module MSignal (MSignal, newMSignal, sendMSignal, receiveMSignal) where 

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

-- | MSignal is an opaque data type
newtype MSignal a = MS (MVar a)


-- | Creates a new MSignal object. This can be used to send and receive signals, possibly containing some data. If you do not want to transmit data, use @'MSignal' ()@
newMSignal :: IO (MSignal a)
newMSignal = MS `liftM` newEmptyMVar

-- | Sends new data to all threads currently running 'receiveMSignal'
sendMSignal :: MSignal a -> a -> IO ()
sendMSignal (MS mv) v = do
	forkIO $ takeMVar mv >> return () -- Cleanup afterwards
	putMVar mv v

-- | Blocks until another threads sends data using 'sendMSignal'. It then returns the sent data.
receiveMSignal :: MSignal a -> IO a
receiveMSignal (MS mv) = 
	readMVar mv
	
