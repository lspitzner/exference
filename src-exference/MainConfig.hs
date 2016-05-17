module MainConfig
  ( testDictRatings 
  , testHeuristicsConfig
  , testBaseInput
  , testBaseInput'
  )
where



import Language.Haskell.Exference.Core ( ExferenceHeuristicsConfig(..) )

import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Language.Haskell.Exts.Parser ( ParseMode (..) )


testDictRatings :: [(String, Float)]
testDictRatings =
  [ (,) "maybe"    0.0
  , (,) "either"   0.0
  , (,) "curry"    0.0
  , (,) "uncurry"  0.0
  --, (,) "compare"  0.0
  --, (,) "minBound" 0.0
  --, (,) "maxBound" 0.0
  , (,) "fmap"     0.0
  -- , (,) "pure"     0.0
  -- , (,) "(<*>)"    0.0
  , (,) "(>>=)"    0.0
  , (,) "mapM"     0.0
  , (,) "sequence" 0.0
  , (,) "foldl"    0.0
  , (,) "foldr"    0.0
  , (,) "concat"   0.0
  , (,) "zip"      0.0
  , (,) "zip3"     0.0
  , (,) "zipWith"  0.0
  , (,) "unzip"    0.0
  , (,) "unzip3"   0.0
  , (,) "repeat"   0.0
  , (,) "Just"     0.0
  --, (,) "(&&)"       0.0
  --, (,) "(||)"       0.0
  ]

testBaseInput :: [(Bool, String)]
testBaseInput = [ (,) True  "GHCEnum"
                , (,) True  "GHCReal"
                , (,) True  "GHCShow"
                , (,) False "ControlMonad"
                , (,) False "DataEither"
                , (,) False "DataList"
                , (,) False "DataMaybe"
                , (,) False "DataTuple"
                , (,) False "DataOrd"
                , (,) False "GHCArr"
                , (,) False "GHCBase"
                , (,) False "GHCFloat"
                , (,) False "GHCList"
                , (,) False "GHCNum"
                , (,) False "GHCST"
                , (,) False "SystemIOError"
                , (,) False "SystemIO"
                , (,) False "TextRead"
                ]

testBaseInput' :: [(ParseMode, String)]
testBaseInput' = map h testBaseInput
  where
    h (shouldBangPattern, s) =
      let exts1 = (if shouldBangPattern then (BangPatterns:) else id)
                  [ UnboxedTuples
                  , TypeOperators
                  , MagicHash
                  , NPlusKPatterns
                  , ExplicitForAll
                  , ExistentialQuantification
                  , TypeFamilies
                  , PolyKinds
                  , DataKinds ]
          exts2 = map EnableExtension exts1
          mode = ParseMode (s++".hs")
                           Haskell2010
                           exts2
                           False
                           False
                           Nothing
                           False
          fname = "./BaseContext/preprocessed/"++s++".hs"
      in (mode, fname)

testHeuristicsConfig :: ExferenceHeuristicsConfig
testHeuristicsConfig = ExferenceHeuristicsConfig
  { heuristics_goalVar               =  0.8 -- tested to .1
  , heuristics_goalCons              =  0.7 -- tested to .1
  , heuristics_goalArrow             =  4.3 -- tested to .1
  , heuristics_goalApp               =  1.9 -- tested to .1
  , heuristics_stepProvidedGood      =  0.22 -- tested to .1
  , heuristics_stepProvidedBad       =  5.0 -- tested to .1
  , heuristics_stepEnvGood           =  6.0 -- tested to .1
  , heuristics_stepEnvBad            = 22.0 -- can improve, but needs testcases
  , heuristics_tempUnusedVarPenalty  =  1.1 -- tested to .1
  , heuristics_tempMultiVarUsePenalty=  6.7 -- tested to .1
  , heuristics_functionGoalTransform =  0.1 -- tested to .1
  , heuristics_unusedVar             = 20.0
  , heuristics_solutionLength        =  0.0153
  }
