#!/usr/bin/env runhaskell

import System.IO
import System.Process
import System.FilePath
import System.Directory
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
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
    
    -- Run the fluxus compiler
    let cmd = "cabal run fluxus -- " ++ inputFile ++ " -o " ++ outputFile
    putStrLn $ "Running: " ++ cmd
    
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
    
    putStrLn $ "Return code: " ++ show exitCode
    putStrLn $ "Stdout: " ++ stdout
    putStrLn $ "Stderr: " ++ stderr
    
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
        (runExitCode, runStdout, runStderr) <- readCreateProcessWithExitCode (shell outputFile) ""
        putStrLn $ "Execution result: " ++ show runExitCode
        putStrLn $ "Execution stdout: " ++ runStdout
        putStrLn $ "Execution stderr: " ++ runStderr
      else do
        putStrLn "Output file does not exist"
        -- Check what files were created
        putStrLn $ "All files in " ++ tmpDir ++ ":"
        allFiles <- listDirectory tmpDir
        mapM_ putStrLn allFiles