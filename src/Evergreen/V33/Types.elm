module Evergreen.V33.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Dict
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version
import Evergreen.V33.PackageStatus
import Http
import Lamdera
import Set
import Url


type PackageStatusFrontend
    = Fetched_
        { updateIndex : Int
        }
    | FetchedAndChecked_
        { updateIndex : Int
        , result : Evergreen.V33.PackageStatus.ReviewResult
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
    }


type alias BackendModel =
    { cachedPackages : Dict.Dict String (AssocList.Dict Elm.Version.Version Evergreen.V33.PackageStatus.PackageStatus)
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


type ToBackend
    = ResetBackend
    | ResetRules
    | LoginRequest String
    | PullRequestRequest Elm.Package.Name
    | RerunPackageRequest Elm.Package.Name Elm.Version.Version


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
        Evergreen.V33.PackageStatus.ReviewResult
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
        Evergreen.V33.PackageStatus.ReviewResult


type ToFrontend
    = Updates (Dict.Dict String (AssocList.Dict Elm.Version.Version PackageStatusFrontend))
