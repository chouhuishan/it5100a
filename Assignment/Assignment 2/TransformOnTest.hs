module TransformOnTest (transformations) where

import Data.Ratio
import JSON
import JSONOps
import Test

-- Comment out any of the following list items if you do not want to test that question

transformations :: TestSuite
transformations =
    TestSuite
        "Transformations"
        [ handy
        ]

handy :: TestGroup
handy = TestGroup "Question 1 and 2: JSON-on-JSON Transformations and Handy JSON Transformations" $ do
    let marks = 5
    ghci "myJson"
    print myJson
    let myJso1 =
            O
                [ ("name", S "Bob")
                , ("id", N 1244)
                , ("friends", L [S "Alice", S "Charlie", Null])
                ,
                    ( "courses"
                    , O
                        [
                            ( "IT5100A"
                            , O
                                [ ("completed", B True)
                                , ("score", N 57)
                                ]
                            )
                        ,
                            ( "IT5100B"
                            , O
                                [ ("completed", B False)
                                , ("score", Null)
                                ]
                            )
                        ]
                    )
                , ("email", Null)
                ]
    let myJso2 =
            O
                [ ("name", S "Bob")
                , ("id", N 1234)
                , ("friends", L [S "Alice", S "Charlie", Null])
                ,
                    ( "courses"
                    , O
                        [
                            ( "IT5100A"
                            , O
                                [ ("completed", B False)
                                , ("score", N 47)
                                ]
                            )
                        ,
                            ( "IT5100B"
                            , O
                                [ ("completed", B True)
                                , ("score", Null)
                                ]
                            )
                        ]
                    )
                , ("email", Null)
                ]
    let myJso3 =
            O
                [ ("name", S "Bob")
                , ("id", N 1234)
                , ("friends", L [S "Alice", S "Charlie"])
                ,
                    ( "courses"
                    , O
                        [
                            ( "IT5100A"
                            , O
                                [ ("completed", B True)
                                , ("score", N 47)
                                ]
                            )
                        , ("IT5100B", O [("completed", B False)])
                        ]
                    )
                ]
    a <- testIt "addAll 10 myJson" myJso1 (addAll 10 myJson) marks
    b <- testIt "negateAll myJson" myJso2 (negateAll myJson) marks
    c <- testIt "filterNull myJson" myJso3 (filterNull myJson) marks
    return [a, b, c]
