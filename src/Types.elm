module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Version exposing (Version)
import Http
import Review.Rule
import Url exposing (Url)
import Zip exposing (Zip)


type alias FrontendModel =
    { key : Key
    , message : String
    }


type PackageStatus
    = FetchedAndChecked
        { version : Version
        , docs : List Elm.Docs.Module
        , elmJson : Elm.Project.PackageInfo
        , result : Result CheckError (List Review.Rule.ReviewError)
        }
    | FetchingZipFailed Version Http.Error


type CheckError
    = ElmJsonMissing
    | ElmJsonIsForApplication


packageVersion : PackageStatus -> Version
packageVersion packageStatus =
    case packageStatus of
        FetchedAndChecked { version } ->
            version

        FetchingZipFailed version _ ->
            version


type alias BackendModel =
    { cachedPackages : Dict String (List PackageStatus)
    , todos : List ( String, Version )
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = GotNewPackagePreviews (Result Http.Error (List ( String, Version )))
    | FetchedZipResult String Version (Result Http.Error ( Zip, List Elm.Docs.Module, Elm.Project.Project ))


type ToFrontend
    = NoOpToFrontend


type alias PackageEndpoint =
    { url : String, hash : String }
