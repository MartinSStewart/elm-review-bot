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
