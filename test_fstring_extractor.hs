#!/usr/bin/env runhaskell

import Data.Text (Text)
import qualified Data.Text as T

-- Extract expressions from f-string content like "Hello, {name}!" -> ["name"]
extractFStringExpressions :: Text -> [Text]
extractFStringExpressions text = extractBraces text []
  where
    extractBraces :: Text -> [Text] -> [Text]
    extractBraces remaining acc
      | T.null remaining = reverse acc
      | otherwise =
          case T.findIndex (== '{') remaining of
            Nothing -> reverse acc
            Just startIdx ->
              case T.findIndex (== '}') (T.drop (startIdx + 1) remaining) of
                Nothing -> reverse acc  -- Malformed f-string, no closing brace
                Just endIdx ->
                  let exprText = T.take endIdx (T.drop (startIdx + 1) remaining)
                      afterExpr = T.drop (startIdx + endIdx + 2) remaining
                  in extractBraces afterExpr (exprText : acc)

main :: IO ()
main = do
  let test = "Arithmetic: 5 + 3 = {5 + 3}"
  print $ extractFStringExpressions (T.pack test)