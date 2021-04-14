module Evergreen.V16.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Dict
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Syntax.Range
import Elm.Version
import Http
import Lamdera
import List.Nonempty
import Review.Fix
import Set
import Url


type PullRequestStatus
    = PullRequestNotSent
    | Sent
    | Failed Http.Error


type alias Error =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Elm.Syntax.Range.Range
    }


type RunRuleResult a
    = ParsingError
    | IncorrectProject
    | FixFailed Review.Fix.Problem
    | NotEnoughIterations
    | RunRuleSuccessful
        { errors : List Error
        , oldElmJson : String
        , newElmJson : String
        }
        a
    | NotAnElm19xPackage
    | DependenciesDontExist (List.Nonempty.Nonempty Elm.Package.Name)


type ReviewResult
    = CouldNotOpenDefaultBranchZip
    | PackageTagNotFound
    | HttpError Http.Error
    | InvalidPackageName
    | RuleErrorsFromDefaultBranch (RunRuleResult PullRequestStatus)
    | RuleErrorsFromTag (RunRuleResult PullRequestStatus)
    | RuleErrorsAndDefaultBranchAndTagMatch (RunRuleResult PullRequestStatus)


type PackageStatusFrontend
    = Fetched_
        { version : Elm.Version.Version
        , updateIndex : Int
        }
    | FetchedAndChecked_
        { version : Elm.Version.Version
        , updateIndex : Int
        , result : ReviewResult
        }
    | FetchingElmJsonAndDocsFailed_ Elm.Version.Version Int Http.Error


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


type PackageStatus
    = Pending Elm.Version.Version Int
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
    | FetchingElmJsonAndDocsFailed Elm.Version.Version Int Http.Error


type alias BackendModel =
    { cachedPackages : Dict.Dict String (AssocList.Dict Elm.Version.Version PackageStatus)
    , clients : Set.Set String
    , updateIndex : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleOrder
    | PressedCreateFork
    | PressedResetBackend
    | PressedResetRules
    | CreateForkResult
        (Result
            Http.Error
            { url : String
            }
        )
    | NoOpFrontendMsg
    | PressedLogin
    | TypedPassword String


type ToBackend
    = ResetBackend
    | ResetRules
    | LoginRequest String


type BackendMsg
    = GotNewPackagePreviews (Result Http.Error (List ( String, Elm.Version.Version )))
    | FetchedElmJsonAndDocs
        { packageName : String
        , version : Elm.Version.Version
        }
        (Result Http.Error ( Elm.Project.Project, List Elm.Docs.Module ))
    | RanNoUnused
        { packageName : String
        , elmJson : Elm.Project.PackageInfo
        , docs : List Elm.Docs.Module
        }
        ReviewResult
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = Updates (Dict.Dict String (List PackageStatusFrontend))
