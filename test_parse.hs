import qualified Data.Text as T
import Fluxus.AST.Common

-- Minimal implementation of parseExpression for testing
parseExpression :: String -> Either String (Located CommonExpr)
parseExpression str
  | null str = Right $ noLoc (CELiteral (LInt 42))
  | "func(" `isPrefixOf` str = 
      let argsStr = take (length str - 1) $ drop 5 str  -- Remove "func(" and ")"
          args = if null argsStr || all (== ' ') argsStr
                 then [] 
                 else let argList = splitOn ',' argsStr
                      in case argList of
                           [] -> []
                           [x] -> if null (filter (/= ' ') x) then [] else [parseArg x]
                           xs -> map parseArg xs
          parseArg argStr = 
            case readsMaybe (filter (/= ' ') argStr) of
              Just (n, "") -> noLoc $ CELiteral $ LInt n
              _ -> noLoc $ CELiteral $ LInt 42  -- fallback for invalid args
      in Right $ noLoc $ CECall (noLoc $ CEVar $ Identifier "func") args
  | otherwise = Right $ noLoc (CELiteral (LInt 42))
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    splitOn _ [] = [""]
    splitOn delim s = 
      let (first, rest) = break (== delim) s
      in case rest of
           [] -> [first]
           (_:xs) -> first : splitOn delim xs
    readsMaybe :: Read a => String -> Maybe (a, String)
    readsMaybe s = case reads s of
                    [x] -> Just x
                    _ -> Nothing

main :: IO ()
main = do
    let testCases = ["func()", "func(1)", "func(1,2)", "func(1,2,3)"]
    mapM_ (\testCase -> do
        let result = parseExpression testCase
        putStrLn $ "Input: " ++ testCase
        case result of
            Right (Located _ (CECall _ args)) -> 
                putStrLn $ "  Success: " ++ show (length args) ++ " args"
            Right other -> 
                putStrLn $ "  Non-call: " ++ show other
            Left err -> 
                putStrLn $ "  Error: " ++ err
        ) testCases