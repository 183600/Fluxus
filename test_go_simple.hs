import Fluxus.Parser.Go.Lexer
import qualified Data.Text as T

main = do
    let content = T.pack "package main"
    case Fluxus.Parser.Go.Lexer.runGoLexer "test.go" content of
        Left err -> print err
        Right tokens -> do
            print ("Successfully tokenized, got " ++ show (length tokens) ++ " tokens")
            mapM_ print tokens