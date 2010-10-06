module SharedData where

import Data.Binary
import Control.Monad (liftM, liftM3)


(+|) t e c = if c then t else e

data Operator = Plus | Minus | Mult | Div
  deriving (Eq, Ord, Enum, Show, Read)

instance Binary Operator where
  put op = putWord8 $ case op of
             Plus  -> 0
             Minus -> 1
             Mult  -> 2
             Div   -> 3
  get = liftM trans getWord8
    where trans 0 = Plus
          trans 1 = Minus
          trans 2 = Mult
          trans 3 = Div

data Operation = Operation Float Operator Float | Stop
  deriving (Eq, Show, Read)

instance Binary Operation where
  put (Operation a op b) = put True >> put a >> put op >> put b
  put Stop               = put False
  get = get >>= liftM3 Operation get get get
                +| return Stop

