module Tests exposing (..)

import Backend
import Elm.Project exposing (Project(..))
import Expect
import Json.Decode
import Review.Project
import Test exposing (..)


suite : Test
suite =
    describe
        "elm-review-bot tests"
        [ test "Only include used modules" <|
            \_ ->
                Backend.project
                    package
                    [ { path = "src/A.elm", source = """module A exposing (..)
                    
import C

a = 0""" }
                    , { path = "src/B.elm", source = """module B exposing (..)

a = 0""" }
                    , { path = "src/C.elm", source = """module C exposing (..)

import
    A.D exposing (..)

a = 0""" }
                    , { path = "src/D.elm", source = """module D exposing (..)

a = 0""" }
                    , { path = "src/A.D.elm", source = """module A.D exposing (..)

a = 0""" }
                    ]
                    []
                    |> Review.Project.modules
                    |> List.length
                    |> Expect.equal 4
        , test "Always include test modules" <|
            \_ ->
                Backend.project
                    package
                    [ { path = "src/A.elm", source = """module A exposing (..)

a = 0""" }
                    , { path = "src/B.elm", source = """module B exposing (..)

a = 0""" }
                    ]
                    [ { path = "tests/C.elm", source = """module C exposing (..)
 
a = 0""" }
                    , { path = "tests/D.elm", source = """module D exposing (..)
 
a = 0""" }
                    ]
                    |> Review.Project.modules
                    |> List.length
                    |> Expect.equal 4
        ]


package : Elm.Project.PackageInfo
package =
    """{
    "type": "package",
    "name": "author/elm-package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "A",
        "B"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""
        |> Json.Decode.decodeString Elm.Project.decoder
        |> (\a ->
                case a of
                    Ok (Package packageInfo) ->
                        packageInfo

                    _ ->
                        Debug.todo "Invalid package elm.json"
           )
