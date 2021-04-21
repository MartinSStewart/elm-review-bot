module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import Github
import Set exposing (Set)


githubAuth_ : String
githubAuth_ =
    ""


githubAuth =
    Github.authToken githubAuth_


adminPassword : String
adminPassword =
    "123"


ignoreList_ : String
ignoreList_ =
    ""


ignoreList : Set String
ignoreList =
    ignoreList_ |> String.split "," |> List.map String.trim |> Set.fromList
