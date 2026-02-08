module HSPC.TypeCheck (typeCheck) where

import HSPC.Parse (ParseError, Program (..))

typeCheck :: Program -> Maybe ParseError
typeCheck _ = Nothing
