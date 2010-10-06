-- |
-- Binary Communicator
-- 
-- This module provides the datatype BinaryCom, which enables you
-- to easily send and receive data to and from a binary source.
-- The transmitted data can be an instance of the 'Binary' class,
-- or you can provide your own Put and Get actions to serialize
-- and parse the binary stream.

module Data.BinaryCom
  (BinaryCom,
   binaryCom, binaryCom2H, binaryComBS,
   send, flushAfter, receive,
   sendPut, receiveGet,
   (+|))
where

import System.IO
import Data.IORef
import Control.Monad (when)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B


-- | 'BinaryCom' type
data BinaryCom = BinaryCom (IORef L.ByteString) -- For reading
                           Handle               -- For writing
                           Bool                 -- Auto-flush when 'send' called

-- | Creates a 'BinaryCom' from a 'Handle' opened for both reading and writing.
-- Be careful not to use the handle afterwards
binaryCom :: (MonadIO m) => Handle -> m BinaryCom
binaryCom h = binaryCom2H h h

-- | Creates a 'BinaryCom' from two 'Handle's: one for reading, one for writing
binaryCom2H :: (MonadIO m) =>
               Handle      -- ^ For reading
            -> Handle      -- ^ For writing
            -> m BinaryCom -- ^ New 'BinaryCom'
binaryCom2H hR hW = do
  inp <- liftIO $ L.hGetContents hR
  binaryComBS inp hW

-- | Creates a 'BinaryCom' from a lazy 'L.ByteString' (for reading) and a 'Handle' (for writing)
binaryComBS :: (MonadIO m) =>
               L.ByteString -- ^ For reading
            -> Handle       -- ^ For writing
            -> m BinaryCom  -- ^ New 'BinaryCom'
binaryComBS inp hW = liftIO $ do
  ref <- newIORef inp
  return $ BinaryCom ref hW True

-- | Sends a serializable value through a 'BinaryCom'
send :: (B.Binary a, MonadIO m) => BinaryCom -> a -> m ()
send b@(BinaryCom _ _ doFlush) val = do
  sendPut b . B.put $ val
  when doFlush $ flush b

-- | Runs a continuation, passing it a binary com with auto-flush deactivated.
--   Flushes when the continuation is finished.
--   It permits not to flush at each call to 'send'.
flushAfter :: (MonadIO m) => BinaryCom -> (BinaryCom -> m ()) -> m ()
flushAfter b@(BinaryCom ref hW _) cont = do
  cont $ BinaryCom ref hW False
  flush b

flush :: (MonadIO m) => BinaryCom -> m ()
flush (BinaryCom _ hW _) = liftIO $ hFlush hW

-- | Receives a serializable value through a 'BinaryCom'
receive :: (B.Binary a, MonadIO m) => BinaryCom -> m a
receive b = receiveGet b B.get

-- | Runs a 'B.Put' monad and sends its result
sendPut :: (MonadIO m) => BinaryCom -> B.Put -> m ()
sendPut (BinaryCom _ hW _) putAction = liftIO $
  L.hPut hW $ B.runPut putAction

-- | Receives a value. Runs a 'B.Get' monad to parse it
receiveGet :: (MonadIO m) => BinaryCom -> B.Get a -> m a
receiveGet (BinaryCom ref _ _) getAction = liftIO $ do
  inp <- readIORef ref
  let (val, inpRest, _) = B.runGetState getAction inp 0
  writeIORef ref inpRest
  return val

-- | A if-then-else, but with the condition as last argument
(+|) :: a -> a -> Bool -> a
(+|) t e c = if c then t else e

