module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.Range exposing (Range)
import Elm.Version exposing (Version)
import Http
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Url exposing (Url)


type alias FrontendModel =
    { state : Dict String (List PackageStatusFrontend)
    , key : Key
    , order : DisplayOrder
    }


type alias Error =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    }


type DisplayOrder
    = RequestOrder
    | Alphabetical


type PackageStatus
    = Pending Version Int
    | Fetched
        { updateIndex : Int
        , docs : List Elm.Docs.Module
        , elmJson : Elm.Project.PackageInfo
        }
    | FetchedAndChecked
        { updateIndex : Int
        , docs : List Elm.Docs.Module
        , elmJson : Elm.Project.PackageInfo
        , result : ReviewResult
        }
    | FetchingElmJsonAndDocsFailed Version Int Http.Error


type PackageStatusFrontend
    = Fetched_
        { version : Version
        , updateIndex : Int
        }
    | FetchedAndChecked_
        { version : Version
        , updateIndex : Int
        , result : ReviewResult
        }
    | FetchingElmJsonAndDocsFailed_ Version Int Http.Error


statusToStatusFrontend : PackageStatus -> Maybe PackageStatusFrontend
statusToStatusFrontend packageStatus =
    case packageStatus of
        Pending _ _ ->
            Nothing

        Fetched a ->
            Fetched_ { version = a.elmJson.version, updateIndex = a.updateIndex } |> Just

        FetchedAndChecked a ->
            FetchedAndChecked_
                { version = a.elmJson.version
                , updateIndex = a.updateIndex
                , result = a.result
                }
                |> Just

        FetchingElmJsonAndDocsFailed version int error ->
            FetchingElmJsonAndDocsFailed_ version int error |> Just


packageVersion : PackageStatus -> Version
packageVersion packageStatus =
    case packageStatus of
        Pending version _ ->
            version

        Fetched { elmJson } ->
            elmJson.version

        FetchedAndChecked { elmJson } ->
            elmJson.version

        FetchingElmJsonAndDocsFailed version _ _ ->
            version


packageVersion_ : PackageStatusFrontend -> Version
packageVersion_ packageStatus =
    case packageStatus of
        Fetched_ { version } ->
            version

        FetchedAndChecked_ { version } ->
            version

        FetchingElmJsonAndDocsFailed_ version _ _ ->
            version


updateIndex : PackageStatusFrontend -> Int
updateIndex packageStatus =
    case packageStatus of
        Fetched_ a ->
            a.updateIndex

        FetchedAndChecked_ a ->
            a.updateIndex

        FetchingElmJsonAndDocsFailed_ _ a _ ->
            a


type alias BackendModel =
    { cachedPackages : Dict String (List PackageStatus)
    , clients : Set String
    , updateIndex : Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ToggleOrder
    | PressedCreateFork
    | CreateForkResult (Result Http.Error { url : String })
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = GotNewPackagePreviews (Result Http.Error (List ( String, Version )))
    | FetchedElmJsonAndDocs
        { packageName : String
        , version : Version
        }
        (Result Http.Error ( Elm.Project.Project, List Elm.Docs.Module ))
    | RanNoUnused
        { packageName : String
        , elmJson : Elm.Project.PackageInfo
        , docs : List Elm.Docs.Module
        }
        ReviewResult
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ReviewResult
    = CouldNotOpenDefaultBranchZip
    | CouldNotOpenTagZip
    | PackageTagNotFound
    | HttpError Http.Error
    | InvalidPackageName
    | NoErrors
    | RuleErrorsFromDefaultBranch RunRuleResult
    | RuleErrorsFromTag RunRuleResult
    | RuleErrorsAndPullRequest { errors : List Error, pullRequestUrl : String }


type RunRuleResult
    = ParsingError
    | IncorrectProject
    | NotEnoughIterations
    | RunRuleSuccessful (List Error) String
    | NotAnElm19xPackage


type ToFrontend
    = Updates (Dict String (List PackageStatusFrontend))


type alias PackageEndpoint =
    { url : String, hash : String }
