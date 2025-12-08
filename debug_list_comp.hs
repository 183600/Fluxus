import qualified Fluxus.Compiler.Driver as Driver

main :: IO ()
main = do
  let config = Driver.defaultConfig {
        Driver.ccSourceLanguage = Driver.Python,
        Driver.ccStopAtCodegen = True,
        Driver.ccKeepIntermediates = True,
        Driver.ccVerboseLevel = 5  -- Increase verbosity
      }
  
  result <- Driver.runCompiler config $ do
    Driver.compileFile "test_list_comp.py"
    
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
    Right (_, state) -> do
      putStrLn "Successfully generated C++ source"
      putStrLn $ "Intermediate files: " ++ show (Driver.csIntermediateFiles state)