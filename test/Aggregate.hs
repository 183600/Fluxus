module Main (main) where

import System.Exit
import System.Process

run :: String -> [String] -> IO ()
run cmd args = do
  code <- rawSystem cmd args
  case code of
    ExitSuccess -> pure ()
    _ -> exitWith code

main :: IO ()
main = do
  run "cabal" ["test", "fluxus-test", "--test-show-details=direct", "--enable-coverage"]
  run "bash" ["scripts/hlint.sh"]
  run "bash" ["scripts/ormolu-check.sh"]
  run "bash" ["scripts/stan.sh"]
  run "bash" ["scripts/weeder.sh"]
  run "bash" ["scripts/doctest.sh"]
  run "bash" ["scripts/check-coverage.sh", "90" ]
