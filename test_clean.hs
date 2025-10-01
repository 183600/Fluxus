-- Clean test of directory scanning functionality
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeFileName, takeExtension, (</>))
import Data.List (isSuffixOf, isPrefixOf)

-- | Check if file has supported extension
hasSupportedExtension :: FilePath -> Bool
hasSupportedExtension file =
  any (`isSuffixOf` file) [".py", ".go"]

-- | Check if path is a hidden directory (to avoid infinite loops)
isHiddenPath :: FilePath -> Bool
isHiddenPath path =
  "." `isPrefixOf` takeFileName path || ".." `isPrefixOf` takeFileName path

-- | Scan directory recursively for code files
scanDirectoryForCodeFiles :: FilePath -> IO [FilePath]
scanDirectoryForCodeFiles dir = do
  -- Skip hidden directories
  when (isHiddenPath dir) $ return []

  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      contents <- getDirectoryContents dir
      let properContents = filter (not . isHiddenPath) contents
      filesWithDirs <- mapM (scanItem dir) properContents
      return $ concat filesWithDirs
  where
    scanItem :: FilePath -> String -> IO [FilePath]
    scanItem basePath item = do
      let fullPath = basePath </> item
      isDir <- doesDirectoryExist fullPath
      if isDir
        then scanDirectoryForCodeFiles fullPath
        else if hasSupportedExtension item
             then return [fullPath]
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