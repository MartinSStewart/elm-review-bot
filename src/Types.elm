module Types exposing (..)

import AssocList
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version exposing (Version)
import Http
import Lamdera exposing (ClientId, SessionId)
import PackageStatus exposing (PackageStatus(..), ReviewResult, ReviewResult_)
import Set exposing (Set)
import Url exposing (Url)


type alias FrontendModel =
    { state : Dict String (AssocList.Dict Version PackageStatusFrontend)
    , key : Key
    , order : DisplayOrder
    , loginStatus : LoginStatus
    }


type LoginStatus
    = NotLoggedIn String
    | LoggedIn


type DisplayOrder
    = RequestOrder
    | Alphabetical


type PackageStatusFrontend
    = Fetched_
        { updateIndex : Int
        }
    | FetchedAndChecked_
        { updateIndex : Int
        , result : ReviewResult
        }
    | FetchingElmJsonAndDocsFailed_ Version Int Http.Error
    | FetchedCheckedAndPullRequestPending_
        { updateIndex : Int
        }
    | FetchedCheckedAndPullRequestSent_
        { updateIndex : Int
        , url : String
        }
    | FetchedCheckedAndPullRequestFailed_
        { updateIndex : Int
        , error : ( String, Http.Error )
        }


statusToStatusFrontend : PackageStatus -> Maybe PackageStatusFrontend
statusToStatusFrontend packageStatus =
    case packageStatus of
        Pending _ _ ->
            Nothing

        Fetched a ->
            Fetched_ { updateIndex = a.updateIndex } |> Just

        FetchedAndChecked a ->
            FetchedAndChecked_
                { updateIndex = a.updateIndex
                , result = a.result
                }
                |> Just

        FetchingElmJsonAndDocsFailed version int error ->
            FetchingElmJsonAndDocsFailed_ version int error |> Just

        FetchedCheckedAndPullRequestPending a ->
            FetchedCheckedAndPullRequestPending_
                { updateIndex = a.updateIndex
                }
                |> Just

        FetchedCheckedAndPullRequestSent a { url } ->
            FetchedCheckedAndPullRequestSent_
                { updateIndex = a.updateIndex
                , url = url
                }
                |> Just

        FetchedCheckedAndPullRequestFailed a error ->
            FetchedCheckedAndPullRequestFailed_
                { updateIndex = a.updateIndex
                , error = error
                }
                |> Just


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

        FetchedCheckedAndPullRequestPending fetchedAndChecked_ ->
            fetchedAndChecked_.elmJson.version

        FetchedCheckedAndPullRequestSent fetchedAndChecked_ _ ->
            fetchedAndChecked_.elmJson.version

        FetchedCheckedAndPullRequestFailed fetchedAndChecked_ _ ->
            fetchedAndChecked_.elmJson.version


updateIndex : PackageStatusFrontend -> Int
updateIndex packageStatus =
    case packageStatus of
        Fetched_ a ->
            a.updateIndex

        FetchedAndChecked_ a ->
            a.updateIndex

        FetchingElmJsonAndDocsFailed_ _ a _ ->
            a

        FetchedCheckedAndPullRequestPending_ a ->
            a.updateIndex

        FetchedCheckedAndPullRequestSent_ a ->
            a.updateIndex

        FetchedCheckedAndPullRequestFailed_ a ->
            a.updateIndex


type alias BackendModel =
    { cachedPackages : Dict String (AssocList.Dict Version PackageStatus)
    , clients : Set String
    , updateIndex : Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ToggleOrder
    | PressedResetBackend
    | NoOpFrontendMsg
    | PressedLogin
    | TypedPassword String
    | PressedCreatePullRequest Elm.Package.Name
    | PressedRerunPackage Elm.Package.Name Version


type ToBackend
    = ResetBackend
    | ResetRules
    | LoginRequest String
    | PullRequestRequest Elm.Package.Name
    | RerunPackageRequest Elm.Package.Name Version


type BackendMsg
    = GotNewPackagePreviews (Result Http.Error (List ( String, Version )))
    | FetchedElmJsonAndDocs
        { packageName : Elm.Package.Name
        , version : Version
        }
        (Result Http.Error ( Elm.Project.Project, List Elm.Docs.Module ))
    | RanNoUnused
        { packageName : Elm.Package.Name
        , elmJson : Elm.Project.PackageInfo
        , docs : List Elm.Docs.Module
        }
        ReviewResult_
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | CreatePullRequestResult Elm.Package.Name Version (Result ( String, Http.Error ) { url : String })
    | ReranPackage
        { packageName : Elm.Package.Name
        , elmJson : Elm.Project.PackageInfo
        , docs : List Elm.Docs.Module
        , updateIndex : Int
        }
        ReviewResult


type ToFrontend
    = Updates (Dict String (AssocList.Dict Version PackageStatusFrontend))


type alias PackageEndpoint =
    { url : String, hash : String }
