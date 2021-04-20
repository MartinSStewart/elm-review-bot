module Evergreen.V36.PackageStatus exposing (..)

import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Syntax.Range
import Elm.Version
import Http
import List.Nonempty
import Review.Fix


type alias Error =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Elm.Syntax.Range.Range
    }


type alias FoundErrors_ =
    { errors : List.Nonempty.Nonempty Error
    , oldElmJson : String
    , newElmJson : String
    }


type RunRuleResult
    = ParsingError (List.Nonempty.Nonempty String)
    | IncorrectProject
    | FixFailed Review.Fix.Problem
    | NotEnoughIterations
    | FoundErrors FoundErrors_
    | NoErrorsFounds
    | NotAnElm19xPackage
    | DependenciesDontExist (List.Nonempty.Nonempty Elm.Package.Name)


type ReviewResult
    = CouldNotOpenDefaultBranchZip
    | PackageTagNotFound
    | HttpError Http.Error
    | RuleErrors RunRuleResult


type alias FetchedAndChecked_ a =
    { updateIndex : Int
    , docs : List Elm.Docs.Module
    , elmJson : Elm.Project.PackageInfo
    , result : a
    }


type PackageStatus
    = Pending Elm.Version.Version Int
    | Fetched
        { updateIndex : Int
        , docs : List Elm.Docs.Module
        , elmJson : Elm.Project.PackageInfo
        }
    | FetchedAndChecked (FetchedAndChecked_ ReviewResult)
    | FetchedCheckedAndPullRequestPending (FetchedAndChecked_ FoundErrors_)
    | FetchedCheckedAndPullRequestSent
        (FetchedAndChecked_ FoundErrors_)
        { url : String
        }
    | FetchedCheckedAndPullRequestFailed (FetchedAndChecked_ FoundErrors_) ( String, Http.Error )
    | FetchingElmJsonAndDocsFailed Elm.Version.Version Int Http.Error


type ReviewResult_
    = CouldNotOpenDefaultBranchZip_
    | PackageTagNotFound_
    | HttpError_ Http.Error
    | RuleErrors_ Bool RunRuleResult
