module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import Github


dummyConfigItem =
    ""


githubAuth_ =
    ""


githubAuth =
    Github.authToken githubAuth_
