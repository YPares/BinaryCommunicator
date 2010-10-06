import SharedData

import Data.BinaryCom
import Network
import System.IO
import Control.Concurrent
import Control.Monad (liftM, forever, when)
import Control.Monad.Trans (liftIO)
import Control.Exception
import Codec.Compression.GZip


performOpe :: Operation -> Maybe Float
performOpe (Operation a op b) = Just $ (case op of
  Plus -> (+)
  Minus -> (-)
  Mult -> (*)
  Div -> (/)) a b
performOpe Stop = Nothing

serveClient :: BinaryCom -> IO ()
serveClient com =
  liftM performOpe (receive com) >>=
    maybe (return ()) (\res -> do
      sendFlush com res
      serveClient com)

forkClient :: (Handle, HostName, PortNumber) -> IO ThreadId
forkClient (hdl,ip,_) = forkIO $
  binaryCom hdl >>= serveClient
  where
    exHdl :: SomeException -> IO ()
    exHdl ex = do putStrLn $ "Connection with client '" ++ ip ++ "' brutally closed."

main = withSocketsDo $
  bracket (listenOn $ PortNumber 6066) sClose $ \sockIn ->
    forever $ accept sockIn >>= forkClient

