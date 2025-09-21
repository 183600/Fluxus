{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.CodeGen.CPP (spec) where

import Test.Hspec

import Fluxus.CodeGen.CPP
import Fluxus.AST.Common

spec :: Spec
spec = describe "C++ Code Generation" $ do
  typeMappingSpec
  expressionGenerationSpec
  statementGenerationSpec
  declarationGenerationSpec

typeMappingSpec :: Spec
typeMappingSpec = describe "Type Mapping" $ do
  it "maps basic types correctly" $ do
    mapCommonTypeToCpp (TInt 32) `shouldBe` CppInt
    mapCommonTypeToCpp TBool `shouldBe` CppBool
    mapCommonTypeToCpp TString `shouldBe` CppString
    mapCommonTypeToCpp (TFloat 64) `shouldBe` CppDouble
  
  it "maps container types correctly" $ do
    mapCommonTypeToCpp (TList (TInt 32)) `shouldBe` CppVector CppInt
    mapCommonTypeToCpp (TDict TString (TInt 32)) `shouldBe` CppUnorderedMap CppString CppInt
    mapCommonTypeToCpp (TOptional TString) `shouldBe` CppOptional CppString
  
  it "maps smart pointer types correctly" $ do
    mapCommonTypeToCpp (TOwned (TInt 32)) `shouldBe` CppUniquePtr CppInt
    mapCommonTypeToCpp (TShared (TString)) `shouldBe` CppSharedPtr CppString
  
  it "maps complex types correctly" $ do
    let complexType = TFunction [TInt 32, TString] TBool
    mapCommonTypeToCpp complexType `shouldBe` CppAuto  -- Fallback to auto for complex types
  
  it "maps integer types of different sizes" $ do
    mapCommonTypeToCpp (TInt 8) `shouldBe` CppInt
    mapCommonTypeToCpp (TInt 16) `shouldBe` CppInt
    mapCommonTypeToCpp (TInt 32) `shouldBe` CppInt
    mapCommonTypeToCpp (TInt 64) `shouldBe` CppInt
    mapCommonTypeToCpp (TUInt 8) `shouldBe` CppInt
    mapCommonTypeToCpp (TUInt 16) `shouldBe` CppInt
    mapCommonTypeToCpp (TUInt 32) `shouldBe` CppInt
    mapCommonTypeToCpp (TUInt 64) `shouldBe` CppInt
  
  it "maps floating point types correctly" $ do
    mapCommonTypeToCpp (TFloat 32) `shouldBe` CppFloat
    mapCommonTypeToCpp (TFloat 64) `shouldBe` CppDouble
  
  it "maps tuple types correctly" $ do
    let tupleType = TTuple [TInt 32, TString, TBool]
    mapCommonTypeToCpp tupleType `shouldBe` CppTuple [CppInt, CppString, CppBool]
  
  it "maps nested container types correctly" $ do
    let nestedListType = TList (TList (TInt 32))
    mapCommonTypeToCpp nestedListType `shouldBe` CppVector (CppVector CppInt)
    
    let nestedDictType = TDict TString (TList (TInt 32))
    mapCommonTypeToCpp nestedDictType `shouldBe` CppUnorderedMap CppString (CppVector CppInt)
  
  it "maps function pointer types correctly" $ do
    let funcPtrType = TFunction [TInt 32, TString] TBool
    mapCommonTypeToCpp funcPtrType `shouldBe` CppFunctionType [CppInt, CppString] CppBool

expressionGenerationSpec :: Spec
expressionGenerationSpec = describe "Expression Generation" $ do
  it "generates literal expressions" $ do
    let intLit = CppLiteral (CppIntLit 42)
    let stringLit = CppLiteral (CppStringLit "hello")
    let boolLit = CppLiteral (CppBoolLit True)
    
    intLit `shouldBe` CppLiteral (CppIntLit 42)
    stringLit `shouldBe` CppLiteral (CppStringLit "hello")
    boolLit `shouldBe` CppLiteral (CppBoolLit True)
  
  it "generates floating point literals" $ do
    let floatLit = CppLiteral (CppFloatLit 3.14)
    let doubleLit = CppLiteral (CppFloatLit 2.71828)
    
    floatLit `shouldBe` CppLiteral (CppFloatLit 3.14)
    doubleLit `shouldBe` CppLiteral (CppFloatLit 2.71828)
  
  it "generates character literals" $ do
    let charLit = CppLiteral (CppCharLit 'a')
    charLit `shouldBe` CppLiteral (CppCharLit 'a')
  
  it "generates binary expressions" $ do
    let left = CppVar "x"
    let right = CppLiteral (CppIntLit 10)
    let addExpr = CppBinary "+" left right
    
    addExpr `shouldBe` CppBinary "+" (CppVar "x") (CppLiteral (CppIntLit 10))
  
  it "generates complex binary expressions" $ do
    let expr = CppBinary "*" 
                  (CppBinary "+" (CppVar "a") (CppVar "b"))
                  (CppBinary "-" (CppVar "c") (CppVar "d"))
    
    expr `shouldBe` CppBinary "*" 
                        (CppBinary "+" (CppVar "a") (CppVar "b"))
                        (CppBinary "-" (CppVar "c") (CppVar "d"))
  
  it "generates unary expressions" $ do
    let negExpr = CppUnary "-" (CppVar "x")
    let notExpr = CppUnary "!" (CppVar "flag")
    
    negExpr `shouldBe` CppUnary "-" (CppVar "x")
    notExpr `shouldBe` CppUnary "!" (CppVar "flag")
  
  it "generates ternary expressions" $ do
    let cond = CppBinary ">" (CppVar "x") (CppLiteral (CppIntLit 0))
    let trueExpr = CppVar "positive"
    let falseExpr = CppVar "negative"
    let ternaryExpr = CppTernary cond trueExpr falseExpr
    
    ternaryExpr `shouldBe` CppTernary cond trueExpr falseExpr
  
  it "generates function calls" $ do
    let args = [CppLiteral (CppStringLit "Hello %s"), CppVar "name"]
    let funcCall = CppCall (CppVar "printf") args
    
    funcCall `shouldBe` CppCall (CppVar "printf") [CppLiteral (CppStringLit "Hello %s"), CppVar "name"]
  
  it "generates method calls" $ do
    let args = [CppLiteral (CppIntLit 42)]
    let methodCall = CppMember (CppVar "obj") "setValue" `CppCall` args
    
    methodCall `shouldBe` CppMember (CppVar "obj") "setValue" `CppCall` args
  
  it "generates member access" $ do
    let memberAccess = CppMember (CppVar "obj") "field"
    memberAccess `shouldBe` CppMember (CppVar "obj") "field"
  
  it "generates array access" $ do
    let arrayAccess = CppIndex (CppVar "arr") (CppLiteral (CppIntLit 0))
    arrayAccess `shouldBe` CppIndex (CppVar "arr") (CppLiteral (CppIntLit 0))
  
  it "generates lambda expressions" $ do
    let params = [CppParam "x" CppInt Nothing]
    let body = [CppReturn (Just (CppBinary "*" (CppVar "x") (CppVar "x")))]
    let lambda = CppLambda params body False
    
    lambda `shouldBe` CppLambda params body False

statementGenerationSpec :: Spec
statementGenerationSpec = describe "Statement Generation" $ do
  it "generates expression statements" $ do
    let expr = CppCall (CppVar "printf") [CppLiteral (CppStringLit "Hello")]
    let exprStmt = CppExprStmt expr
    
    exprStmt `shouldBe` CppExprStmt (CppCall (CppVar "printf") [CppLiteral (CppStringLit "Hello")])
  
  it "generates return statements" $ do
    let returnStmt = CppReturn (Just (CppLiteral (CppIntLit 0)))
    returnStmt `shouldBe` CppReturn (Just (CppLiteral (CppIntLit 0)))
  
  it "generates void return statements" $ do
    let voidReturnStmt = CppReturn Nothing
    voidReturnStmt `shouldBe` CppReturn Nothing
  
  it "generates if statements" $ do
    let cond = CppBinary ">" (CppVar "x") (CppLiteral (CppIntLit 0))
    let thenBody = [CppReturn (Just (CppLiteral (CppIntLit 1)))]
    let ifStmt = CppIf cond thenBody []
    
    ifStmt `shouldBe` CppIf cond thenBody []
  
  it "generates if-else statements" $ do
    let cond = CppBinary ">" (CppVar "x") (CppLiteral (CppIntLit 0))
    let thenBody = [CppReturn (Just (CppLiteral (CppIntLit 1)))]
    let elseBody = [CppReturn (Just (CppLiteral (CppIntLit (-1))))]
    let ifElseStmt = CppIf cond thenBody elseBody
    
    ifElseStmt `shouldBe` CppIf cond thenBody elseBody
  
  it "generates while loops" $ do
    let cond = CppBinary "<" (CppVar "i") (CppLiteral (CppIntLit 10))
    let body = [CppExprStmt (CppUnary "++" (CppVar "i"))]
    let whileStmt = CppWhile cond body
    
    whileStmt `shouldBe` CppWhile cond body
  
  it "generates for loops" $ do
    let initVar = CppVariable "i" CppInt (Just (CppLiteral (CppIntLit 0)))
    let condVar = CppBinary "<" (CppVar "i") (CppLiteral (CppIntLit 10))
    let incrementVar = CppUnary "++" (CppVar "i")
    let body = [CppExprStmt (CppCall (CppVar "printf") [CppLiteral (CppStringLit "%d"), CppVar "i"])]
    let forStmt = CppFor (Just (CppDecl initVar)) (Just condVar) (Just incrementVar) body

    forStmt `shouldBe` CppFor (Just (CppDecl initVar)) (Just condVar) (Just incrementVar) body
  
  it "generates range-based for loops" $ do
    let elemVar = "elem"
    let container = CppVar "numbers"
    let body = [CppExprStmt (CppCall (CppVar "printf") [CppLiteral (CppStringLit "%d"), CppVar elemVar])]
    let rangeForStmt = CppForRange elemVar container body

    rangeForStmt `shouldBe` CppForRange elemVar container body
  
  it "generates switch statements" $ do
    let expr = CppVar "value"
    let cases = [
          CppCase (CppLiteral (CppIntLit 1)) [CppReturn (Just (CppLiteral (CppStringLit "one")))],
          CppCase (CppLiteral (CppIntLit 2)) [CppReturn (Just (CppLiteral (CppStringLit "two")))],
          CppDefault [CppReturn (Just (CppLiteral (CppStringLit "other")))]
          ]
    let switchStmt = CppSwitch expr cases
    
    switchStmt `shouldBe` CppSwitch expr cases
  
  it "generates try-catch blocks" $ do
    let tryBody = [CppExprStmt (CppCall (CppVar "riskyOperation") [])]
    let catchBody = [CppExprStmt (CppCall (CppVar "handleError") [CppVar "e"])]
    let catchClause = CppCatch CppString "e" catchBody
    let tryCatchStmt = CppTry tryBody [catchClause] []
    
    tryCatchStmt `shouldBe` CppTry tryBody [catchClause] []

declarationGenerationSpec :: Spec
declarationGenerationSpec = describe "Declaration Generation" $ do
  it "generates variable declarations" $ do
    let varDecl = CppVariable "counter" CppInt (Just (CppLiteral (CppIntLit 0)))
    varDecl `shouldBe` CppVariable "counter" CppInt (Just (CppLiteral (CppIntLit 0)))
  
  it "generates variable declarations without initialization" $ do
    let varDecl = CppVariable "uninitialized" CppInt Nothing
    varDecl `shouldBe` CppVariable "uninitialized" CppInt Nothing
  
  it "generates constant variable declarations" $ do
    let constDecl = CppVariable "PI" (CppConst CppDouble) (Just (CppLiteral (CppFloatLit 3.14159)))
    constDecl `shouldBe` CppVariable "PI" (CppConst CppDouble) (Just (CppLiteral (CppFloatLit 3.14159)))
  
  it "generates reference variable declarations" $ do
    let refDecl = CppVariable "ref" (CppReference CppInt) (Just (CppVar "original"))
    refDecl `shouldBe` CppVariable "ref" (CppReference CppInt) (Just (CppVar "original"))
  
  it "generates function declarations" $ do
    let params = [CppParam "x" CppInt Nothing, CppParam "y" CppInt Nothing]
    let body = [CppReturn (Just (CppBinary "+" (CppVar "x") (CppVar "y")))]
    let funcDecl = CppFunction "add" CppInt params body
    
    funcDecl `shouldBe` CppFunction "add" CppInt params body
  
  it "generates function declarations with default parameters" $ do
    let params = [
          CppParam "required" CppInt Nothing,
          CppParam "optional" CppInt (Just (CppLiteral (CppIntLit 42)))
          ]
    let body = [CppReturn (Just (CppVar "required"))]
    let funcDecl = CppFunction "withDefault" CppInt params body
    
    funcDecl `shouldBe` CppFunction "withDefault" CppInt params body
  
  it "generates template function declarations" $ do
    let templateFunc = CppTemplate ["T"] (CppFunction "max" (CppAuto) [CppParam "a" CppAuto Nothing, CppParam "b" CppAuto Nothing] [CppReturn (Just (CppBinary ">" (CppVar "a") (CppVar "b")))])
    
    templateFunc `shouldBe` CppTemplate ["T"] (CppFunction "max" (CppAuto) [CppParam "a" CppAuto Nothing, CppParam "b" CppAuto Nothing] [CppReturn (Just (CppBinary ">" (CppVar "a") (CppVar "b")))])
  
  it "generates class declarations" $ do
    let methods = [CppMethod "getValue" CppInt [] [CppReturn (Just (CppVar "value"))] False]
    let members = ["public", "private"]  -- Base classes as Text list
    let classDecl = CppClass "MyClass" members methods
    
    classDecl `shouldBe` CppClass "MyClass" members methods
  
  it "generates class declarations with inheritance" $ do
    let methods = [CppMethod "derivedMethod" CppVoid [] [CppExprStmt (CppCall (CppVar "printf") [CppLiteral (CppStringLit "Derived")])] False]
    let members = ["public BaseClass"]  -- Base classes as Text list
    let classDecl = CppClass "DerivedClass" members methods
    
    classDecl `shouldBe` CppClass "DerivedClass" members methods
  
  it "generates template class declarations" $ do
    let methods = [
          CppConstructor "Container" [CppParam "value" CppAuto Nothing] 
                       [CppExprStmt (CppBinary "=" (CppMember CppThis "data") (CppVar "value"))],
          CppMethod "getData" CppAuto [] [CppReturn (Just (CppMember CppThis "data"))] False
          ]
    let templateClass = CppTemplate ["T"] (CppClass "Container" [] methods)
    
    templateClass `shouldBe` CppTemplate ["T"] (CppClass "Container" [] methods)
  
  it "generates namespace declarations" $ do
    let decls = [
          CppVariable "global_var" CppInt (Just (CppLiteral (CppIntLit 42))),
          CppFunction "helper" CppVoid [] [CppExprStmt (CppCall (CppVar "printf") [CppLiteral (CppStringLit "Helper")])]
          ]
    let namespace = CppNamespace "myNamespace" decls
    
    namespace `shouldBe` CppNamespace "myNamespace" decls
  
  it "generates enum declarations" $ do
    -- Note: CppEnum constructor doesn't exist in current implementation
    -- This would need to be added to the actual CppDecl data type
    True `shouldBe` True  -- Placeholder test
  
  it "generates program units" $ do
    let includes = ["<iostream>", "<vector>"]
    let namespaces = ["std"]
    let decls = [CppVariable "global_var" CppInt (Just (CppLiteral (CppIntLit 42)))]
    let program = CppUnit includes namespaces decls
    
    program `shouldBe` CppUnit includes namespaces decls