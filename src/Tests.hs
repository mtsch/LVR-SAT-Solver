import Evaluate
import Types

import Test.HUnit
import Test.QuickCheck

import qualified Data.IntMap.Strict as IM

-- Use runTestTT to run tests

valuation = IM.fromList [(1, TRUE), (2, FALSE), (3, NA)]

-- Test basic evaluations.
testEval = test [ "test1" ~: "+1" ~: TRUE  ~=? eval (Lit 1)
                , "test2" ~: "+2" ~: FALSE ~=? eval (Lit 2)
                , "test3" ~: "+3" ~: NA    ~=? eval (Lit 3)
                , "test4" ~: "-1" ~: FALSE ~=? eval (Not 1)
                , "test5" ~: "-2" ~: TRUE  ~=? eval (Not 2)
                , "test6" ~: "-3" ~: NA    ~=? eval (Not 3)
                ]
    where eval = evalLit valuation

testSatisfiedClause = test [ "1" ~: "empty" ~:
                             True ~=? sat []
                           , "2" ~: "+1 | -1" ~:
                             True ~=? sat [Lit 1, Not 1]
                           , "3" ~: "+2 | -2" ~:
                             True ~=? sat [Lit 2, Not 2]
                           , "4" ~: "-1 | 2" ~:
                             False ~=? sat [Not 1, Lit 2]
                           , "5" ~: "-1 | 2" ~:
                             True ~=? sat [Not 1, Lit 2, Not 1, Lit 1]
                           , "6" ~: "3 | 1" ~:
                             True ~=? sat [Lit 3, Lit 1]
                           , "7" ~: "3 | -3 | 1" ~:
                             True ~=? sat [Lit 3, Not 3, Lit 1]
                           ]
    where sat = clauseSatisfied valuation

testSatisfiedFormula = test [ "1" ~: "empty" ~:
                              True ~=? sat []
                            , "2" ~: "+1 & -2" ~:
                              True ~=? sat [[Lit 1], [Not 2]]
                            , "3" ~: "+1 | -1 & -2 & 2" ~:
                              False ~=? sat [[Lit 1, Not 1], [Not 2], [Lit 2]]
                            ]
    where sat = formulaSatisfied valuation

testSatisfiable = test [ "empty" ~: "empty formula 1" ~:
                         Satisfied [] ~=? satisfy 10 []
                       , "empty" ~: "empty formula 2" ~:
                         Satisfied [] ~=? satisfy 10 [[]]
                       , "empty" ~: "empty formula 3" ~:
                         Satisfied [] ~=? satisfy 10 [[], [], [], []]
                       , "simple" ~: "simple formula 1" ~:
                         Satisfied [(1, TRUE)] ~=? satisfy 10 [[Lit 1]]
                       , "simple" ~: "simple formula 2" ~:
                         Satisfied [(1, FALSE)] ~=? satisfy 10 [[Not 1]]
                       , "simple" ~: "simple formula 2" ~:
                         Satisfied [(7, TRUE)] ~=? satisfy 10 [[Lit 7]]
                       , "simple" ~: "simple formula 2" ~:
                         Satisfied [(10, FALSE)] ~=? satisfy 10 [[Not 10]]
                       , "simple" ~: "satisfiable 1" ~:
                         True ~=? (isSatisfied $ satisfy 10 [[Not 1, Lit 1]])
                       , "simple" ~: "satisfiable 2" ~:
                         False ~=? (isSatisfied $ satisfy 10 [[Not 1], [Lit 1]])
                       , "simple" ~: "simple formula 3" ~:
                         Satisfied [(1, FALSE), (2, TRUE)] ~=?
                                   satisfy 10 [[Not 1], [Lit 2]]
                       ]
