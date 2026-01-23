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
    -- There should be <testName>.pas and <testName>.golden
    --  in sampleProgramsLocation
    testName :: FilePath,
    expectedExit :: Int
  }

sampleProgramsLocation :: FilePath
sampleProgramsLocation = "test/sample_programs/"

testSuite :: [TestSpec]
testSuite =
  [ TestSpec "Exit code 0" "ints_0_exit_code" 0,
    TestSpec "Exit code 255" "ints_255_exit_code" 255,
    TestSpec "Exit code > 255" "ints_big_exit_code" 255,
    TestSpec "Halt after Halt" "ints_halt_after_halt" 0,
    TestSpec "Add literal ints" "operators_add_two_ints" 200,
    TestSpec "Multiply literal ints" "operators_multiply_two_ints" 40,
    TestSpec "Divide(div) literal ints" "operators_divide_two_ints" 2,
    TestSpec "Brackets with literal ints" "operators_brackets" 26,
    TestSpec "Unary minus with literal ints" "operators_unary_minus_int" 75,
    TestSpec "Unary plus with literal ints" "operators_unary_plus_int" 25,
    TestSpec "Or between bools" "operators_bool_or" 0,
    TestSpec "And between bools" "operators_bool_and" 0,
    TestSpec "Negating bools" "operators_bool_not" 0,
    TestSpec "{} comments" "comments_curly_brace" 12,
    TestSpec "// comments" "comments_single_line" 12,
    TestSpec "Single variable" "variables_single_var" 10,
    TestSpec "Boolean variable" "variables_bool" 0,
    TestSpec "IF" "if_no_else" 30,
    TestSpec "IF ELSE" "if_else" 40,
    TestSpec "IF ELSE IF" "if_else_if" 40,
    TestSpec "IF with complex cond" "if_complex_cond" 40
  ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "Compiler Integration Tests"
      [testCase (name t) (runIntegrationTest t) | t <- testSuite]

runIntegrationTest :: TestSpec -> Assertion
runIntegrationTest spec = do
  source <- readFile (sampleProgramsLocation ++ testName spec ++ ".pas")

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
        ("Exit code mismatch for " ++ testName spec ++ "\nStderr: " ++ stderr)
        (expectedExit spec)
        actualCode

      goldenOut <- readFile (sampleProgramsLocation ++ testName spec ++ ".golden")
      assertEqual
        ("StdOut mismatch for " ++ testName spec ++ "\nStderr: " ++ stderr)
        goldenOut
        stdout
