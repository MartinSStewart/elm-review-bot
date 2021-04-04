module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import Github


dummyConfigItem =
    ""


githubAuth_ =
    "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"


githubAuth =
    Github.authToken githubAuth_
