module Evergreen.V21.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Dict
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version
import Evergreen.V21.PackageStatus
import Http
import Lamdera
import Set
import Url


type PackageStatusFrontend
    = Fetched_
        { version : Elm.Version.Version
        , updateIndex : Int
        }
    | FetchedAndChecked_
        { version : Elm.Version.Version
        , updateIndex : Int
        , result : Evergreen.V21.PackageStatus.ReviewResult
        }
    | FetchingElmJsonAndDocsFailed_ Elm.Version.Version Int Http.Error
    | FetchedCheckedAndPullRequestPending_
        { version : Elm.Version.Version
        , updateIndex : Int
        }
    | FetchedCheckedAndPullRequestSent_
        { version : Elm.Version.Version
        , updateIndex : Int
        , url : String
        }
    | FetchedCheckedAndPullRequestFailed_
        { version : Elm.Version.Version
        , updateIndex : Int
        , error : Http.Error
        }


type DisplayOrder
    = RequestOrder
    | Alphabetical


type LoginStatus
    = NotLoggedIn String
    | LoggedIn


type alias FrontendModel =
    { state : Dict.Dict String (List PackageStatusFrontend)
    , key : Browser.Navigation.Key
    , order : DisplayOrder
    , loginStatus : LoginStatus
    }


type alias BackendModel =
    { cachedPackages : Dict.Dict String (AssocList.Dict Elm.Version.Version Evergreen.V21.PackageStatus.PackageStatus)
    , clients : Set.Set String
    , updateIndex : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleOrder
    | PressedResetBackend
    | PressedResetRules
    | NoOpFrontendMsg
    | PressedLogin
    | TypedPassword String
    | PressedCreatePullRequest Elm.Package.Name


type ToBackend
    = ResetBackend
    | ResetRules
    | LoginRequest String
    | PullRequestRequest Elm.Package.Name


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
        Evergreen.V21.PackageStatus.ReviewResult
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId
    | CreatePullRequestResult
        Elm.Package.Name
        Elm.Version.Version
        (Result
            Http.Error
            { url : String
            }
        )


type ToFrontend
    = Updates (Dict.Dict String (List PackageStatusFrontend))
