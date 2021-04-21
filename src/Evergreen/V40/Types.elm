module Evergreen.V40.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Dict
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version
import Evergreen.V40.PackageStatus
import Http
import Lamdera
import Set
import Time
import Url


type PackageStatusFrontend
    = Fetched_
        { updateIndex : Int
        }
    | FetchedAndChecked_
        { updateIndex : Int
        , result : Evergreen.V40.PackageStatus.ReviewResult
        }
    | FetchingElmJsonAndDocsFailed_ Elm.Version.Version Int Http.Error
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


type DisplayOrder
    = RequestOrder
    | Alphabetical


type LoginStatus
    = NotLoggedIn String
    | LoggedIn


type alias FrontendModel =
    { state : Dict.Dict String (AssocList.Dict Elm.Version.Version PackageStatusFrontend)
    , key : Browser.Navigation.Key
    , order : DisplayOrder
    , loginStatus : LoginStatus
    , ignoreList : Set.Set String
    }


type alias BackendModel =
    { cachedPackages : Dict.Dict String (AssocList.Dict Elm.Version.Version Evergreen.V40.PackageStatus.PackageStatus)
    , clients : Set.Set String
    , updateIndex : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleOrder
    | PressedResetBackend
    | NoOpFrontendMsg
    | PressedLogin
    | TypedPassword String
    | PressedCreatePullRequest Elm.Package.Name
    | PressedRerunPackage Elm.Package.Name Elm.Version.Version
    | PressedFetchNewPackages


type ToBackend
    = ResetBackend
    | ResetRules
    | LoginRequest String
    | PullRequestRequest Elm.Package.Name
    | RerunPackageRequest Elm.Package.Name Elm.Version.Version
    | FetchNewPackagesRequest


type BackendMsg
    = GotNewPackagePreviews (Result Http.Error (List ( String, Elm.Version.Version )))
    | FetchedElmJsonAndDocs
        { packageName : Elm.Package.Name
        , version : Elm.Version.Version
        }
        (Result Http.Error ( Elm.Project.Project, List Elm.Docs.Module ))
    | RanNoUnused
        { packageName : Elm.Package.Name
        , elmJson : Elm.Project.PackageInfo
        , docs : List Elm.Docs.Module
        }
        Evergreen.V40.PackageStatus.ReviewResult_
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | CreatePullRequestResult
        Elm.Package.Name
        Elm.Version.Version
        (Result
            ( String, Http.Error )
            { url : String
            }
        )
    | ReranPackage
        { packageName : Elm.Package.Name
        , elmJson : Elm.Project.PackageInfo
        , docs : List Elm.Docs.Module
        , updateIndex : Int
        }
        Evergreen.V40.PackageStatus.ReviewResult
    | TimeElapsed Time.Posix


type ToFrontend
    = Updates (Dict.Dict String (AssocList.Dict Elm.Version.Version PackageStatusFrontend))
    | FirstUpdate
        { cachedPackages : Dict.Dict String (AssocList.Dict Elm.Version.Version PackageStatusFrontend)
        , ignoreList : Set.Set String
        }
