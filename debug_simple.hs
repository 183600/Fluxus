import System.Process
import System.Directory
import System.Exit
import System.IO

main :: IO ()
main = do
  -- Create temporary directory
  tmpDir <- readProcess "mktemp" ["-d", "fluxus-memory-test-XXXX"] ""
  let tmpDir' = init tmpDir  -- remove newline
  
  let inputFile = tmpDir' ++ "/memory.py"
  let outputFile = tmpDir' ++ "/memory.cpp"
  
  -- Write the test code
  writeFile inputFile $ unlines
    [ "result = 0"
    , "i = 0"
    , "while i < 1000000:"
    , "    result = result + (i % 1000)"
    , "    i = i + 1"
    , "print(f\"Processed {result} elements\")"
    ]
  
  putStrLn $ "Input file: " ++ inputFile
  putStrLn $ "Output file: " ++ outputFile
  putStrLn $ "Directory: " ++ tmpDir'
  
  -- List files before compilation
  putStrLn "Files before compilation:"
  filesBefore <- getDirectoryContents tmpDir'
  mapM_ putStrLn filesBefore
  
  -- Run the fluxus compiler
  let cmd = "cabal run fluxus -- " ++ inputFile ++ " -o " ++ outputFile
  putStrLn $ "Running: " ++ cmd
  
  (exitCode, stdout, stderr) <- readProcessWithExitCode "bash" ["-c", cmd] ""
  
  putStrLn $ "Return code: " ++ show exitCode
  putStrLn $ "Stdout: " ++ stdout
  putStrLn $ "Stderr: " ++ stderr
  
  -- List files after compilation
  putStrLn "Files after compilation:"
  filesAfter <- getDirectoryContents tmpDir'
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
    else do
      putStrLn "Output file does not exist"
      -- Check what files were created
      putStrLn $ "All files in " ++ tmpDir' ++ ":"
      allFiles <- getDirectoryContents tmpDir'
      mapM_ putStrLn allFiles
  
  -- Clean up
  removeDirectoryRecursive tmpDir'