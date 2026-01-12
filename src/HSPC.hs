module HSPC (compile) where

import Data.ByteString qualified as BS
import HSPC.CodeGen (generate)
import HSPC.Parse (ParseError, parse)
import HSPC.Tokenize (tokenize)

compile :: String -> Either ParseError BS.ByteString
compile str = fmap (BS.pack . generate) (parse $ tokenize str)
