{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.IO (writeFile)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = if null args then "test_concurrent_fib.go" else head args
  
  putStrLn $ "Testing C++ generation from: " ++ inputFile
  
  -- Create a simple script that uses the compiler
  writeFile "run_test.hs" $ unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "import qualified Fluxus.Compiler.Driver as Driver"
    , "import System.Environment (getArgs)"
    , "import Control.Monad (when)"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  args <- getArgs"
    , "  let inputFile = if null args then \"test_concurrent_fib.go\" else head args"
    , "  putStrLn $ \"Generating C++ from: \" ++ inputFile"
    , "  let config = Driver.defaultConfig {"
    , "        Driver.ccSourceLanguage = Driver.Go,"
    , "        Driver.ccStopAtCodegen = True,"
    , "        Driver.ccKeepIntermediates = True,"
    , "        Driver.ccVerboseLevel = 3"
    , "      }"
    , "  result <- Driver.runCompiler config $ do"
    , "    Driver.compileFile inputFile"
    , "  case result of"
    , "    Left err -> do"
    , "      putStrLn $ \"Error: \" ++ show err"
    , "    Right (_, state) -> do"
    , "      putStrLn $ \"Successfully generated C++ source\""
    , "      putStrLn $ \"Intermediate files: \" ++ show (Driver.csIntermediateFiles state)"
    ]
  
  (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["runghc", "run_test.hs", inputFile] ""
  
  case exitCode of
    ExitSuccess -> do
      putStrLn "C++ generation test completed successfully"
      putStrLn stdout
    _ -> do
      putStrLn $ "Error: " ++ stderr