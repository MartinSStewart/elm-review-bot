module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Bytes
import Dict
import Elm.Docs
import Elm.Project
import Elm.Syntax.Range
import Elm.Version
import Http
import Lamdera
import Set
import Url


type alias Error = 
    { message : String
    , ruleName : String
    , filePath : String
    , details : (List String)
    , range : Elm.Syntax.Range.Range
    }


type PackageStatusFrontend
    = FetchedAndChecked_ 
    { version : Elm.Version.Version
    , index : Int
    , errors : (List Error)
    }
    | FetchingZipFailed_ Elm.Version.Version Int Http.Error


type DisplayOrder
    = RequestOrder
    | Alphabetical


type alias FrontendModel =
    { state : (Dict.Dict String (List PackageStatusFrontend))
    , key : Browser.Navigation.Key
    , order : DisplayOrder
    }


type PackageStatus
    = FetchedAndChecked 
    { version : Elm.Version.Version
    , index : Int
    , docs : (List Elm.Docs.Module)
    , elmJson : Elm.Project.PackageInfo
    , errors : (List Error)
    }
    | FetchingZipFailed Elm.Version.Version Int Http.Error


type alias BackendModel =
    { cachedPackages : (Dict.Dict String (List PackageStatus))
    , todos : (List (String, Elm.Version.Version))
    , clients : (Set.Set String)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleOrder
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = GotNewPackagePreviews (Result Http.Error (List (String, Elm.Version.Version)))
    | FetchedZipResult String Elm.Version.Version Int (Result Http.Error (Bytes.Bytes, (List Elm.Docs.Module)))
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = Updates (Dict.Dict String (List PackageStatusFrontend))