module Main (main) where

import Data.ByteString qualified as BS
import HSPC (compile)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Posix.Files (ownerModes, setFileMode)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

data TestSpec = TestSpec
  { name :: String,
    srcPath :: FilePath,
    goldenPath :: FilePath,
    expectedExit :: Int
  }

sampleProgramsLocation :: FilePath
sampleProgramsLocation = "test/sample_programs/"

testSuite :: [TestSpec]
testSuite =
  [ TestSpec "Exit code 0" "ints_0_exit_code.pas" "/dev/null" 0,
    TestSpec "Exit code 255" "ints_255_exit_code.pas" "/dev/null" 255,
    TestSpec "Halt after Halt" "ints_halt_after_halt.pas" "/dev/null" 0,
    TestSpec "Add literal ints" "operators_add_two_ints.pas" "/dev/null" 200,
    TestSpec "Multiply literal ints" "operators_multiply_two_ints.pas" "/dev/null" 200,
    TestSpec "Divide(div) literal ints" "operators_divide_two_ints.pas" "/dev/null" 2
  ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Compiler Integration Tests"
      [testCase (name t) (runIntegrationTest t) | t <- testSuite]

runIntegrationTest :: TestSpec -> Assertion
runIntegrationTest spec = do
  source <- readFile (sampleProgramsLocation ++ srcPath spec)

  case compile source of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right elf -> withSystemTempFile "test_bin" $ \tempPath handle -> do
      BS.hPut handle elf
      hClose handle
      setFileMode tempPath ownerModes

      (exitCode, stdout, stderr) <- readProcessWithExitCode tempPath [] ""

      let actualCode = case exitCode of
            ExitSuccess -> 0
            ExitFailure n -> n

      assertEqual
        ("Exit code mismatch for " ++ srcPath spec ++ "\nStderr: " ++ stderr)
        (expectedExit spec)
        actualCode

      goldenOut <- readFile (goldenPath spec)
      assertEqual
        ("StdOut mismatch for " ++ srcPath spec ++ "\nStderr: " ++ stderr)
        goldenOut
        stdout
