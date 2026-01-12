module HSPC (compile) where

-- import Data.ByteString qualified as BS
-- import Debug.Trace (trace)
import HSPC.Tokenize (HSPCToken, tokenize)

compile :: String -> [HSPCToken]
compile str = tokenize str
