module Main (main) where

import Data.ByteString qualified as BS
import HSPC (compile)
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)

makeExecutable :: FilePath -> IO ()
makeExecutable path = do
  perms <- getPermissions path
  let newPerms = setOwnerExecutable True perms
  setPermissions path newPerms

example :: [String]
example =
  [ "program CustomExitCode;",
    "begin",
    "// this is a comment",
    "  Halt(42);",
    "end."
  ]

main :: IO ()
main = do
  putStrLn (unlines example)
  let parseResult = compile (unlines example)
  case parseResult of
    Right program -> do
      let path = "a.out"
      BS.writeFile path program
      makeExecutable path
    Left err -> putStrLn (show err)
  return ()
