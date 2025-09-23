module Test.Fluxus.ConvertCommand (spec) where

import Test.Hspec
import System.Process
import System.Directory
import System.FilePath
import System.IO
import System.Exit
import Data.List (isSuffixOf)
import Control.Monad (when, forM_)
import Data.Char (isSpace)
import System.Posix.Files (FileMode, ownerReadMode, ownerWriteMode, ownerExecuteMode, groupReadMode, groupExecuteMode, otherReadMode, otherExecuteMode, setFileMode)

spec :: Spec
spec = describe "Fluxus Convert Command Tests" $ do
  it "runs fluxus --python -O2 convert without errors" $ do
    -- Clean up any existing output directory
    outputDirExists <- doesDirectoryExist "test/python-testsoutput"
    when outputDirExists $ removeDirectoryRecursive "test/python-testsoutput"

    -- Run fluxus convert command
    (exitCode, stdout, stderr) <- readProcessWithExitCode "fluxus" ["--python", "-O2", "convert", "test/python-tests", "-o", "test/python-testsoutput"] ""

    -- Check that the command succeeded
    exitCode `shouldBe` ExitSuccess

    -- Check that output directory was created
    outputDirExists <- doesDirectoryExist "test/python-testsoutput"
    outputDirExists `shouldBe` True

    putStrLn $ "Fluxus convert stdout: " ++ stdout
    putStrLn $ "Fluxus convert stderr: " ++ stderr

  it "executes Python files and compares outputs with compiled executables" $ do
    -- Get all Python files in test/python-tests
    pythonFiles <- listDirectory "test/python-tests"
    let pyFiles = filter (".py" `isSuffixOf`) pythonFiles

    -- Get all files in test/python-testsoutput (should be executables)
    outputExists <- doesDirectoryExist "test/python-testsoutput"
    when outputExists $ do
      outputFiles <- listDirectory "test/python-testsoutput"

      -- For each Python file, run it and capture output
      forM_ pyFiles $ \pyFile -> do
        let pyPath = "test/python-tests" </> pyFile
        let expectedFile = pyPath -<.> ".expected"

        -- Check if expected file exists
        expectedExists <- doesFileExist expectedFile
        when expectedExists $ do
          -- Run Python file
          (exitCodePy, stdoutPy, stderrPy) <- readProcessWithExitCode "python" [pyPath] ""

          -- Read expected output
          expectedContent <- readFile expectedFile

          -- Compare Python output with expected
          let cleanPyOutput = trimWhitespace stdoutPy
          let cleanExpected = trimWhitespace expectedContent

          putStrLn $ "Python file: " ++ pyFile
          putStrLn $ "Python stdout: " ++ cleanPyOutput
          putStrLn $ "Expected: " ++ cleanExpected

          exitCodePy `shouldBe` ExitSuccess
          cleanPyOutput `shouldBe` cleanExpected

      -- For each output file, run it and compare with Python output
      forM_ outputFiles $ \outputFile -> do
        let outputPath = "test/python-testsoutput" </> outputFile
        let baseName = takeBaseName outputFile
        let pyFile = baseName ++ ".py"
        let pyPath = "test/python-tests" </> pyFile

        -- Check if corresponding Python file exists
        pyExists <- doesFileExist pyPath
        when pyExists $ do
          -- Make output file executable if it's not already
          setFileMode outputPath executableMode

          -- Run the compiled executable
          (exitCodeExe, stdoutExe, stderrExe) <- readProcessWithExitCode outputPath [] ""

          -- Run the original Python file
          (exitCodePy, stdoutPy, stderrPy) <- readProcessWithExitCode "python" [pyPath] ""

          -- Compare outputs
          let cleanExeOutput = trimWhitespace stdoutExe
          let cleanPyOutput = trimWhitespace stdoutPy

          putStrLn $ "Executable: " ++ outputFile
          putStrLn $ "Executable stdout: " ++ cleanExeOutput
          putStrLn $ "Python stdout: " ++ cleanPyOutput
          putStrLn $ "Executable stderr: " ++ stderrExe
          putStrLn $ "Python stderr: " ++ stderrPy

          exitCodeExe `shouldBe` ExitSuccess
          exitCodePy `shouldBe` ExitSuccess
          cleanExeOutput `shouldBe` cleanPyOutput

  where
    -- Helper function to trim whitespace from a string
    trimWhitespace :: String -> String
    trimWhitespace = unlines . map trimLine . lines
      where
        trimLine :: String -> String
        trimLine = dropWhileEnd isSpace . dropWhile isSpace

        dropWhileEnd :: (a -> Bool) -> [a] -> [a]
        dropWhileEnd p = foldr (\x acc -> if p x && null acc then [] else x:acc) []

    -- File permissions helper
    executableMode :: FileMode
    executableMode = ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode
                      .|. groupReadMode .|. groupExecuteMode
                      .|. otherReadMode .|. otherExecuteMode