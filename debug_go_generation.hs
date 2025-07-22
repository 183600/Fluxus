{- cabal:
build-depends: base, text, containers, fluxus
ghc-options: -Wall -Werror
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    putStrLn "=== Testing Go Code Generation ==="
    
    -- Read the test file
    content <- TIO.readFile "simple_test.go"
    putStrLn "=== Input Go Code ==="
    TIO.putStrLn content
    
    putStrLn "=== Done ==="