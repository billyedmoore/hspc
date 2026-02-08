module HSPC (compile) where

import Data.ByteString qualified as BS
import HSPC.CodeGen (generate)
import HSPC.Parse (ParseError, parse)
import HSPC.Tokenize (tokenize)
import HSPC.TypeCheck (typeCheck)

-- Little helper to allow typeCheck to return Maybe
-- which imo is more intuitive.
raiseToEither :: (a -> Maybe b) -> a -> Either b ()
raiseToEither f x =
  case f x of
    Just err -> Left err
    Nothing -> Right ()

compile :: String -> Either ParseError BS.ByteString
compile str = do
  let tokens = tokenize str
  ast <- parse tokens
  raiseToEither typeCheck ast
  let byteCode = generate ast
  return $ BS.pack byteCode
