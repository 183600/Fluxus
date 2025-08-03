import qualified Data.Text as T
import Fluxus.Parser.Go.Parser
import Fluxus.Parser.Go.Lexer

main :: IO ()
main = do
  let goCode = "for i := 1; i <= 3; i++ { println(i) }"
  case parseGoStatement (Located undefined) goCode of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right stmt -> putStrLn $ "Parsed: " ++ show stmt