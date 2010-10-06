import SharedData

import Data.BinaryCom
import Control.Monad (when, liftM)
import Control.Monad.Trans (liftIO)
import Network
import System.Environment (getArgs)
import System.IO
import Control.Exception


handleConn :: BinaryCom -> IO ()
handleConn com = do
  putStrLn "Operation?"
  ope <- liftM read (liftIO $ getLine)
  sendFlush com ope
  when (ope /= Stop) $ do
    res <- receive com
    putStrLn $ show (res :: Float)
    handleConn com

main = withSocketsDo $ do
  (addr:_) <- getArgs
  hdl <- connectTo addr (PortNumber 6066)
  binaryCom hdl >>= handleConn

