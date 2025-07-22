#!/bin/bash

echo "Building Fluxus compiler..."

# Build the project
if stack build; then
    echo "✓ Build successful"
    
    # Create a simple test runner
    cat > test_runner.hs << 'EOF'
import qualified Fluxus.Compiler.Driver as Driver
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = if null args then "test_concurrent_fib.go" else head args
  
  putStrLn $ "Generating C++ from: " ++ inputFile
  
  let config = Driver.defaultConfig { 
        Driver.ccSourceLanguage = Driver.Go,
        Driver.ccStopAtCodegen = True,
        Driver.ccKeepIntermediates = True,
        Driver.ccVerboseLevel = 3
      }
  
  result <- Driver.runCompiler config $ do
    Driver.compileFile inputFile
  
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
    Right (_, state) -> do
      putStrLn $ "Successfully generated C++ source"
      putStrLn $ "Intermediate files: " ++ show (Driver.csIntermediateFiles state)
EOF
    
    # Run the test
    echo "Running C++ generation test..."
    if stack runghc test_runner.hs test_concurrent_fib.go; then
        echo "✓ C++ generation test completed"
        
        # Check for generated file
        if [ -f "test_concurrent_fib.cpp" ]; then
            echo "✓ C++ file generated successfully"
            echo ""
            echo "=== Generated C++ Code Preview ==="
            echo "File: test_concurrent_fib.cpp"
            echo "Size: $(wc -l test_concurrent_fib.cpp) lines"
            echo ""
            head -30 test_concurrent_fib.cpp
        else
            echo "✗ C++ file not found"
        fi
    else
        echo "✗ C++ generation test failed"
    fi
    
else
    echo "✗ Build failed"
    exit 1
fi