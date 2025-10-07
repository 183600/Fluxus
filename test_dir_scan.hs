-- Test directory scanning functionality

import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import System.FilePath (takeFileName, takeExtension, (</>))
import Data.List (isSuffixOf)

-- | Check if file has supported extension
hasSupportedExtension :: FilePath -> Bool
hasSupportedExtension file =
  any (`isSuffixOf` file) [".py", ".go"]

-- | Scan directory recursively for code files
scanDirectoryForCodeFiles :: FilePath -> IO [FilePath]
scanDirectoryForCodeFiles dir = do
  contents <- getDirectoryContents dir
  filesWithDirs <- mapM (scanItem dir) contents
  let allFiles = concat filesWithDirs
  when (not $ null allFiles) $
    putStrLn $ "Scanned directory " ++ dir ++ ": found " ++ show (length allFiles) ++ " code files"
  return allFiles
  where
    scanItem :: FilePath -> String -> IO [FilePath]
    scanItem basePath item = do
      let fullPath = basePath </> item
      isDir <- doesDirectoryExist fullPath
      if isDir
        then scanDirectoryForCodeFiles fullPath
        else if hasSupportedExtension item
             then do
               putStrLn $ "  Found code file: " ++ fullPath
               return [fullPath]
             else return []

-- | Main test function
main :: IO ()
main = do
  putStrLn "Testing directory scanning functionality..."

  -- Test with test directory
  let testDir = "test_dir_scanning"

  isDir <- doesDirectoryExist testDir
  unless isDir $ do
    putStrLn $ "Error: Directory " ++ testDir ++ " does not exist"
    putStrLn "Creating test directory structure first..."
    return ()

  putStrLn $ "Scanning directory: " ++ testDir
  foundFiles <- scanDirectoryForCodeFiles testDir

  putStrLn $ "Total code files found: " ++ show (length foundFiles)

  mapM_ putStrLn foundFiles

  putStrLn "Directory scanning test completed!"