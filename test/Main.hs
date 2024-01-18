-- | Tests for Assignment 5.
--original code from Assignent 1
module Main where

import A5.Skel

import Test.HUnit
import System.Exit

-- TODO: Delete these tests, and write your own for 'evalExpr'.
a5Tests :: Test
a5Tests = TestList
    [  --testcase for freevars
      TestCase (assertEqual "Testing freeVars (Var x)" ["X"] (freeVars (Var "X")))
    , TestCase (assertEqual "Testing freeVars (Lam x e)" ["Y"] (freeVars (Lam "X" (Var "Y"))))
    , TestCase (assertEqual "Testing freeVars (App e1 e2)" ["X", "Y"] (freeVars (App (Var "X") (Var "Y"))))
    --, TestCase (assertEqual "Testing freeVars (Let x e1 e2)" ["X", "Y"] (freeVars (Let "X" (Var "Y") (Var "X"))))
    , TestCase (assertEqual "Testing freeVars (Pair e1 e2)" ["X", "Y"] (freeVars (Pair (Var "X") (Var "Y"))))
    , TestCase (assertEqual "Testing freeVars (Fst e)" ["X"] (freeVars (Fst (Var "X"))))
    , TestCase (assertEqual "Testing freeVars (Snd e)" ["X"] (freeVars (Snd (Var "X"))))
    , TestCase (assertEqual "Testing freeVars (If e1 e2 e3)" ["X", "Y", "Z"] (freeVars (If (Var "X") (Var "Y") (Var "Z"))))
    , TestCase (assertEqual "Testing free:Vars (Succ e)" ["X"] (freeVars (Succ (Var "X"))))
    , TestCase (assertEqual "Testing freeVars (Pred e)" ["X"] (freeVars (Pred (Var "X"))))
    , TestCase (assertEqual "Testing freeVars (IsZero e)" ["X"] (freeVars (IsZero (Var "X"))))
    -- testcases for freeTpVars
    ,  TestCase (assertEqual "Testing freeTpVars (TpVar x)" ["X"] (freeTpVars (TpVar "X")))
    , TestCase (assertEqual "Testing freeTpVars (FnTp (TpVar \"a\") (TpVar \"b\"))" ["a", "b"] (freeTpVars (FnTp (TpVar "a") (TpVar "b"))))
    , TestCase (assertEqual "Testing freeTpVars (PairTp (TpVar \"x\") (TpVar \"y\"))" ["x", "y"] (freeTpVars (PairTp (TpVar "x") (TpVar "y"))))
    , TestCase (assertEqual "Testing freeTpVars BoolTp" [] (freeTpVars BoolTp))
    , TestCase (assertEqual "Testing freeTpVars NatTp" [] (freeTpVars NatTp))
    , TestCase (assertEqual "Testing freeTpVars UnitTp" [] (freeTpVars UnitTp))
    --test cases for freeSchemeVars
    , TestCase (assertEqual "Testing freeSchemeVars (Mono (TpVar \"a\"))" ["a"] (freeSchemeVars (Mono (TpVar "a"))))
    , TestCase (assertEqual "Testing freeSchemeVars (Mono (FnTp (TpVar \"b\") (TpVar \"c\")))" ["b", "c"] (freeSchemeVars (Mono (FnTp (TpVar "b") (TpVar "c")))))
    --, TestCase (assertEqual "Testing freeSchemeVars (Poly [\"x\", \"y\"] (PairTp (TpVar \"x\") (TpVar \"y\")))" ["x", "y"] (freeSchemeVars (Poly ["x", "y"] (PairTp (TpVar "x") (TpVar "y")))))
    , TestCase (assertEqual "Testing freeSchemeVars (Poly [] BoolTp)" [] (freeSchemeVars (Poly [] BoolTp)))
    , TestCase (assertEqual "Testing freeSchemeVars (Poly [] UnitTp)" [] (freeSchemeVars (Poly [] UnitTp)))
    --test cases for freeCtxVars
    , TestCase (assertEqual "Testing freeCtxVars with an empty context" [] (freeCtxVars []))
    , TestCase (assertEqual "Testing freeCtxVars with a monomorphic context" ["a", "b"] (freeCtxVars [("x", Mono (TpVar "a")), ("y", Mono (TpVar "b"))]))
    --, TestCase (assertEqual "Testing freeCtxVars with a polymorphic context" ["c", "d"] (freeCtxVars [("m", Poly ["c"] (TpVar "c")), ("n", Poly ["d"] (TpVar "d"))]))
    --, TestCase (assertEqual "Testing freeCtxVars with a mixed context" ["e", "f", "g"] (freeCtxVars [("p", Mono (TpVar "e")), ("q", Poly ["f"] (TpVar "f")), ("r", Poly ["g"] (TpVar "g"))]))
    , TestCase (assertEqual "Testing composeSub with non-overlapping substitutions"
              [("a", TpVar "b"), ("c", TpVar "d")]
              (composeSub [("a", TpVar "b")] [("c", TpVar "d")]))
              
    --, TestCase (assertEqual "Testing composeSub with overlapping substitutions"
    --          [("a", TpVar "b"), ("c", TpVar "d")]
    --          (composeSub [("a", TpVar "e"), ("c", TpVar "f")] [("e", TpVar "b"), ("f", TpVar "d")]))
              
    , TestCase (assertEqual "Testing composeSub with empty substitutions"
              [("a", TpVar "b"), ("c", TpVar "d")]
              (composeSub [] [("a", TpVar "b"), ("c", TpVar "d")]))
              
    , TestCase (assertEqual "Testing thinSub with non-overlapping variables"
              [("a", TpVar "b"), ("c", TpVar "d")]
              (thinSub ["e", "f"] [("a", TpVar "b"), ("c", TpVar "d"), ("e", TpVar "g"), ("f", TpVar "h")]))
              
    --, TestCase (assertEqual "Testing thinSub with overlapping variables"
    --          [("c", TpVar "d")]
    --          (thinSub ["a", "b"] [("a", TpVar "c"), ("b", TpVar "d"), ("c", TpVar "e")]))
    --test cases for freshen
  , TestCase (assertEqual "Testing freshen with no avoid list" "x" (freshen "x" [])) 
  , TestCase (assertEqual "Testing freshen with one avoided variable" "x'" (freshen "x" ["x"]))
  , TestCase (assertEqual "Testing freshen with multiple avoided variables" "x''" (freshen "x" ["x", "x'"]))
  , TestCase (assertEqual "Testing freshen with no need for renaming" "y" (freshen "y" ["x", "z"]))

    
    ]
  
-- Run the test suite. Please do not touch this!
main :: IO ()
main = do
    counts <- runTestTT a5Tests
    if errors counts + failures counts == 0 then
      exitSuccess
    else
      exitFailure
