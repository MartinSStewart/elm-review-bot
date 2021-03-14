module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    }


type alias BackendModel =
    { cachedPackages : Dict String { version : String }
    , cachedCount : Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type alias PackagePreview =
    { version : Version }


type BackendMsg
    = NoOpBackendMsg
    | GotNewPackagePreviews (Result Http.Error (Dict String (List Version)))


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


versionToString : Version -> String
versionToString { major, minor, patch } =
    String.fromInt major ++ "." ++ String.fromInt minor ++ "." ++ String.fromInt patch


type ToFrontend
    = NoOpToFrontend


type alias PackageEndpoint =
    { url : String, hash : String }
