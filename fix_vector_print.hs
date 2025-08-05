-- Helper function to check if an expression is a vector
isVectorExpr :: CppExpr -> Bool
isVectorExpr (CppInitList (CppVector _) _) = True
isVectorExpr _ = False

-- Simple function to print vector elements by iterating through them
generateVectorPrint :: CppExpr -> CppCodeGen CppExpr
generateVectorPrint expr = do
  addInclude "<algorithm>"
  addInclude "<iterator>"
  -- Use std::for_each to iterate through vector elements and print them
  let printExpr = CppCall (CppVar "std::for_each") 
        [ CppCall (CppMember expr "begin") []
        , CppCall (CppMember expr "end") []
        , CppLambda [CppParam "elem" CppAuto Nothing] 
          [CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppVar "elem")]
        ]
  return printExpr
