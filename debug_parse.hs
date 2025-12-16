import Test.Fluxus.QuickCheckProperties (parseExpression)

main :: IO ()
main = do
    let result = parseExpression "func()"
    putStrLn $ "Result: " ++ show result