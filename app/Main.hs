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
  let parseResult = compile (unlines example)
  case parseResult of
    Right ast -> putStrLn (show ast)
    Left err -> putStrLn (show err)
  return ()
