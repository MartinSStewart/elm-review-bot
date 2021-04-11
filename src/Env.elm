module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import Github


githubAuth_ : String
githubAuth_ =
    ""


githubAuth =
    Github.authToken githubAuth_


adminPassword : String
adminPassword =
    "123"
