module Main (main) where

import HSPC (compile)

example :: [String]
example =
  [ "program CustomExitCode;",
    "begin",
    "// this is a comment",
    "\tHalt(42);",
    "end."
  ]

main :: IO ()
main = do
  putStrLn (unlines example)
  putStrLn (show (compile (unlines example)))
  return ()
