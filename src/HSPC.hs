module HSPC (compile) where

-- import Data.ByteString qualified as BS
-- import Debug.Trace (trace)

import HSPC.Parse (AST, ParseError, parse)
import HSPC.Tokenize (tokenize)

compile :: String -> Either ParseError AST
compile str = parse $ tokenize str
