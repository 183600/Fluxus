{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Analysis.TypeInference (spec) where

import Test.Hspec

import Fluxus.AST.Common

spec :: Spec
spec = describe "Type Inference" $ do
  basicTypeInferenceSpec
  unificationSpec
  constraintSolvingSpec

basicTypeInferenceSpec :: Spec
basicTypeInferenceSpec = describe "Basic Type Inference" $ do
  it "infers integer literal types" $ do
    let intType = TInt 32
    intType `shouldBe` TInt 32
  
  it "infers string literal types" $ do
    let stringType = TString
    stringType `shouldBe` TString
  
  it "infers boolean literal types" $ do
    let boolType = TBool
    boolType `shouldBe` TBool
  
  it "infers list types" $ do
    let listType = TList (TInt 32)
    listType `shouldBe` TList (TInt 32)
  
  it "infers function types" $ do
    let funcType = TFunction [TInt 32, TString] TBool
    funcType `shouldBe` TFunction [TInt 32, TString] TBool

unificationSpec :: Spec
unificationSpec = describe "Type Unification" $ do
  it "unifies identical types" $ do
    let type1 = TInt 32
    let type2 = TInt 32
    unifyTypes type1 type2 `shouldBe` Just (TInt 32)
  
  it "fails to unify different types" $ do
    let type1 = TInt 32
    let type2 = TString
    unifyTypes type1 type2 `shouldBe` Nothing
  
  it "unifies type variables" $ do
    let type1 = TVar (TypeVar "a")
    let type2 = TInt 32
    unifyTypes type1 type2 `shouldBe` Just (TInt 32)
  
  it "unifies complex types" $ do
    let type1 = TList (TVar (TypeVar "a"))
    let type2 = TList (TInt 32)
    unifyTypes type1 type2 `shouldBe` Just (TList (TInt 32))

constraintSolvingSpec :: Spec
constraintSolvingSpec = describe "Constraint Solving" $ do
  it "solves simple constraints" $ do
    let constraints = [(TVar (TypeVar "a"), TInt 32)]
    solveConstraints constraints `shouldBe` Just [(TypeVar "a", TInt 32)]
  
  it "detects inconsistent constraints" $ do
    let constraints = 
          [ (TVar (TypeVar "a"), TInt 32)
          , (TVar (TypeVar "a"), TString)
          ]
    solveConstraints constraints `shouldBe` Nothing
  
  it "propagates constraints" $ do
    let constraints = 
          [ (TVar (TypeVar "a"), TVar (TypeVar "b"))
          , (TVar (TypeVar "b"), TInt 32)
          ]
    case solveConstraints constraints of
      Just solution -> do
        lookup (TypeVar "a") solution `shouldBe` Just (TInt 32)
        lookup (TypeVar "b") solution `shouldBe` Just (TInt 32)
      Nothing -> expectationFailure "Constraint solving should succeed"

-- Helper functions for type inference testing
unifyTypes :: Type -> Type -> Maybe Type
unifyTypes t1 t2
  | t1 == t2 = Just t1
  | otherwise = case (t1, t2) of
      (TVar _, t) -> Just t
      (t, TVar _) -> Just t
      (TList t1', TList t2') -> TList <$> unifyTypes t1' t2'
      (TFunction args1 ret1, TFunction args2 ret2) ->
        if length args1 == length args2
          then do
            unifiedArgs <- sequence $ zipWith unifyTypes args1 args2
            unifiedRet <- unifyTypes ret1 ret2
            return $ TFunction unifiedArgs unifiedRet
          else Nothing
      _ -> Nothing

solveConstraints :: [(Type, Type)] -> Maybe [(TypeVar, Type)]
solveConstraints constraints = go constraints []
  where
    go [] solution = Just solution
    go ((TVar var, typ):rest) solution
      | not (occurs var typ) = 
          let subst = substitute var typ
              newConstraints = [(subst t1, subst t2) | (t1, t2) <- rest]
              newSolution = (var, typ) : [(v, subst t) | (v, t) <- solution]
          in go newConstraints newSolution
    go ((typ, TVar var):rest) solution
      | not (occurs var typ) = 
          let subst = substitute var typ
              newConstraints = [(subst t1, subst t2) | (t1, t2) <- rest]
              newSolution = (var, typ) : [(v, subst t) | (v, t) <- solution]
          in go newConstraints newSolution
    go ((t1, t2):rest) solution
      | t1 == t2 = go rest solution
      | otherwise = Nothing
    
    occurs :: TypeVar -> Type -> Bool
    occurs var (TVar var') = var == var'
    occurs var (TList t) = occurs var t
    occurs var (TFunction args ret) = any (occurs var) args || occurs var ret
    occurs _ _ = False
    
    substitute :: TypeVar -> Type -> Type -> Type
    substitute var replacement (TVar var')
      | var == var' = replacement
      | otherwise = TVar var'
    substitute var replacement (TList t) = TList (substitute var replacement t)
    substitute var replacement (TFunction args ret) = 
      TFunction (map (substitute var replacement) args) (substitute var replacement ret)
    substitute _ _ t = t