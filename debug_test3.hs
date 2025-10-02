import Test.Hspec
import System.IO.Temp
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Compiler.Driver
import qualified Fluxus.Compiler.Config as Config

main :: IO ()
main = hspec $ do
  describe "Debug test" $ do
    it "should compile and run memory-intensive code" $ do
      withSystemTempDirectory "fluxus-memory-test-" $ \tmpDir -> do
        let inputFile = tmpDir </> "memory.py"
        let outputFile = tmpDir </> "memory.cpp"
        
        -- Write the test code
        TIO.writeFile inputFile $ T.unlines
          [ "result = 0"
          , "i = 0"
          , "while i < 1000000:"
          , "    result = result + (i % 1000)"
          , "    i = i + 1"
          , "print(f\"Processed {result} elements\")"
          ]
        
        putStrLn $ "Input file: " ++ inputFile
        putStrLn $ "Output file: " ++ outputFile
        putStrLn $ "Directory: " ++ tmpDir
        
        -- List files before compilation
        putStrLn "Files before compilation:"
        filesBefore <- listDirectory tmpDir
        mapM_ putStrLn filesBefore
        
        let config = Config.defaultConfig
              { Config.ccInputFiles = [inputFile]
              , Config.ccOutputPath = Just outputFile
              , Config.ccSourceLanguage = Config.Python
              , Config.ccCppStandard = "c++20"
              , Config.ccOptimizationLevel = Config.O3
              }
        
        putStrLn "Running compiler..."
        result <- runCompiler (convertConfigToDriver config) (return ())
        
        case result of
          Right _ -> do
            putStrLn "Compilation succeeded"
            
            -- List files after compilation
            putStrLn "Files after compilation:"
            filesAfter <- listDirectory tmpDir
            mapM_ putStrLn filesAfter
            
            -- Check if output file exists
            outputExists <- doesFileExist outputFile
            putStrLn $ "Output file exists: " ++ show outputExists
            
            if outputExists
              then do
                fileType <- readProcess "file" [outputFile] ""
                putStrLn $ "File type: " ++ fileType
                
                -- Try to run it
                (runExitCode, runStdout, runStderr) <- readProcessWithExitCode outputFile [] ""
                putStrLn $ "Execution result: " ++ show runExitCode
                putStrLn $ "Execution stdout: " ++ runStdout
                putStrLn $ "Execution stderr: " ++ runStderr
                
                runExitCode `shouldBe` ExitSuccess
                runStdout `shouldContain` "Processed"
              else do
                putStrLn "Output file does not exist"
                -- Check what files were created
                putStrLn $ "All files in " ++ tmpDir ++ ":"
                allFiles <- listDirectory tmpDir
                mapM_ putStrLn allFiles
                expectationFailure "Output file was not created"
          Left err -> do
            putStrLn $ "Compilation failed: " ++ show err
            expectationFailure $ "Compilation failed: " ++ show err