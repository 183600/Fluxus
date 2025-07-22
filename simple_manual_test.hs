-- Manual test to display Go parser AST
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
    putStrLn "=== Testing Go Parser AST Output ==="
    
    -- Read the func with params no return example  
    content <- TIO.readFile "debug_func_params_no_return.go"
    putStrLn "Input Go code:"
    TIO.putStrLn content
    putStrLn ""
    
    -- Test parsing
    case runGoLexer (T.pack "debug_func_params_no_return.go") content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Lexer successful: " ++ show (length tokens) ++ " tokens"
            
            case runGoParser (T.pack "debug_func_params_no_return.go") tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> do
                    putStrLn "Parser successful!"
                    putStrLn "AST structure:"
                    print ast