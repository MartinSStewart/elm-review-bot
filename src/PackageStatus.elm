module PackageStatus exposing (..)

import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Syntax.Range exposing (Range)
import Elm.Version exposing (Version)
import Http
import List.Nonempty exposing (Nonempty)
import Review.Fix


type alias FetchedAndChecked_ a =
    { updateIndex : Int
    , docs : List Elm.Docs.Module
    , elmJson : Elm.Project.PackageInfo
    , result : a
    }


mapFetchedAndChecked : (a -> b) -> FetchedAndChecked_ a -> FetchedAndChecked_ b
mapFetchedAndChecked mapFunc fetchedAndChecked_ =
    { updateIndex = fetchedAndChecked_.updateIndex
    , docs = fetchedAndChecked_.docs
    , elmJson = fetchedAndChecked_.elmJson
    , result = mapFunc fetchedAndChecked_.result
    }


type PackageStatus
    = Pending Version Int
    | Fetched
        { updateIndex : Int
        , docs : List Elm.Docs.Module
        , elmJson : Elm.Project.PackageInfo
        }
    | FetchedAndChecked (FetchedAndChecked_ ReviewResult)
    | FetchedCheckedAndPullRequestPending (FetchedAndChecked_ FoundErrors_)
    | FetchedCheckedAndPullRequestSent (FetchedAndChecked_ FoundErrors_) { url : String }
    | FetchedCheckedAndPullRequestFailed (FetchedAndChecked_ FoundErrors_) Http.Error
    | FetchingElmJsonAndDocsFailed Version Int Http.Error


pullRequestSent : { url : String } -> PackageStatus -> PackageStatus
pullRequestSent pullRequest packageStatus =
    case packageStatus of
        FetchedCheckedAndPullRequestPending fetchedAndChecked_ ->
            FetchedCheckedAndPullRequestSent fetchedAndChecked_ pullRequest

        _ ->
            packageStatus


pullRequestFailed : Http.Error -> PackageStatus -> PackageStatus
pullRequestFailed error packageStatus =
    case packageStatus of
        FetchedCheckedAndPullRequestPending fetchedAndChecked_ ->
            FetchedCheckedAndPullRequestFailed fetchedAndChecked_ error

        _ ->
            packageStatus


pullRequestPending : FoundErrors_ -> PackageStatus -> PackageStatus
pullRequestPending foundErrors_ packageStatus =
    case packageStatus of
        FetchedAndChecked fetchedAndChecked_ ->
            FetchedCheckedAndPullRequestPending
                (mapFetchedAndChecked (always foundErrors_) fetchedAndChecked_)

        _ ->
            packageStatus


type ReviewResult
    = CouldNotOpenDefaultBranchZip
    | PackageTagNotFound
    | HttpError Http.Error
    | RuleErrors RunRuleResult


type RunRuleResult
    = ParsingError
    | IncorrectProject
    | FixFailed Review.Fix.Problem
    | NotEnoughIterations
    | FoundErrors FoundErrors_
    | NoErrorsFounds
    | NotAnElm19xPackage
    | DependenciesDontExist (Nonempty Elm.Package.Name)


type alias FoundErrors_ =
    { errors : Nonempty Error, oldElmJson : String, newElmJson : String }


getRunRuleResult : ReviewResult -> Maybe RunRuleResult
getRunRuleResult reviewResult =
    case reviewResult of
        CouldNotOpenDefaultBranchZip ->
            Nothing

        PackageTagNotFound ->
            Nothing

        HttpError error ->
            Nothing

        RuleErrors runRuleResult ->
            Just runRuleResult


setRunRuleResult : ReviewResult -> Maybe RunRuleResult
setRunRuleResult reviewResult =
    case reviewResult of
        CouldNotOpenDefaultBranchZip ->
            Nothing

        PackageTagNotFound ->
            Nothing

        HttpError error ->
            Nothing

        RuleErrors runRuleResult ->
            Just runRuleResult


mapRunRuleResultInReviewResult :
    (RunRuleResult -> RunRuleResult)
    -> ReviewResult
    -> ReviewResult
mapRunRuleResultInReviewResult mapFunc reviewResult =
    case reviewResult of
        CouldNotOpenDefaultBranchZip ->
            CouldNotOpenDefaultBranchZip

        PackageTagNotFound ->
            PackageTagNotFound

        HttpError error ->
            HttpError error

        RuleErrors runRuleResult ->
            mapFunc runRuleResult |> RuleErrors


type alias Error =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    }


removeComments : Elm.Docs.Module -> Elm.Docs.Module
removeComments docs =
    { name = docs.name
    , comment = ""
    , unions = List.map (\union -> { union | comment = "" }) docs.unions
    , aliases = List.map (\alias -> { alias | comment = "" }) docs.aliases
    , values = List.map (\value -> { value | comment = "" }) docs.values
    , binops = List.map (\binop -> { binop | comment = "" }) docs.binops
    }
