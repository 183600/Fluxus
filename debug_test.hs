#!/usr/bin/env runghc

-- Simple test to verify debugging works
import Fluxus.Debug.Logger (enableDebug, debugLog, setBreakpoint, checkBreakpoint)

main :: IO ()
main = do
  putStrLn "Testing debug functionality..."

  -- Enable debug mode
  enableDebug

  -- Test logging
  debugLog "Debug mode enabled"

  -- Set a breakpoint
  setBreakpoint "test-point"

  -- Check breakpoint (should pause if debugging enabled)
  checkBreakpoint "test-point"

  debugLog "Debug test completed successfully"
  putStrLn "Debug test finished"