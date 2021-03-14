module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Url exposing (Url)
import Version exposing (MajorVersion, Version)
import Zip exposing (Zip)


type alias FrontendModel =
    { key : Key
    , message : String
    }


type PackageStatus
    = FetchingZip Version
    | FetchedZip Version Zip
    | IsChecked Version Zip
    | Failed Version Http.Error


packageVersion : PackageStatus -> Version
packageVersion packageStatus =
    case packageStatus of
        FetchingZip version ->
            version

        FetchedZip version _ ->
            version

        IsChecked version _ ->
            version

        Failed version _ ->
            version


type alias BackendModel =
    { cachedPackages : Dict ( String, MajorVersion ) PackageStatus
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg
    | GotNewPackagePreviews (Result Http.Error (Dict String (List Version)))
    | FetchedZipResult String Version (Result Http.Error Zip)


type ToFrontend
    = NoOpToFrontend


type alias PackageEndpoint =
    { url : String, hash : String }
