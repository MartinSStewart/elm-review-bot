module Evergreen.V3.Types exposing (..)

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
    = Fetched_ 
    { version : Elm.Version.Version
    , updateIndex : Int
    }
    | FetchedAndChecked_ 
    { version : Elm.Version.Version
    , updateIndex : Int
    , errors : (List Error)
    }
    | FetchingZipFailed_ Elm.Version.Version Int Http.Error
    | FetchingElmJsonAndDocsFailed_ Elm.Version.Version Int Http.Error


type DisplayOrder
    = RequestOrder
    | Alphabetical


type alias FrontendModel =
    { state : (Dict.Dict String (List PackageStatusFrontend))
    , key : Browser.Navigation.Key
    , order : DisplayOrder
    }


type PackageStatus
    = Pending Elm.Version.Version Int
    | Fetched 
    { version : Elm.Version.Version
    , updateIndex : Int
    , docs : (List Elm.Docs.Module)
    , elmJson : Elm.Project.PackageInfo
    }
    | FetchedAndChecked 
    { version : Elm.Version.Version
    , updateIndex : Int
    , docs : (List Elm.Docs.Module)
    , elmJson : Elm.Project.PackageInfo
    , errors : (List Error)
    }
    | FetchingZipFailed Elm.Version.Version Int Http.Error
    | FetchingElmJsonAndDocsFailed Elm.Version.Version Int Http.Error


type alias BackendModel =
    { cachedPackages : (Dict.Dict String (List PackageStatus))
    , clients : (Set.Set String)
    , updateIndex : Int
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
    | FetchedElmJsonAndDocs 
    { packageName : String
    , version : Elm.Version.Version
    } (Result Http.Error (Elm.Project.Project, (List Elm.Docs.Module)))
    | FetchedZipResult 
    { packageName : String
    , version : Elm.Version.Version
    } Elm.Project.PackageInfo (List Elm.Docs.Module) (Result Http.Error Bytes.Bytes)
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = Updates (Dict.Dict String (List PackageStatusFrontend))