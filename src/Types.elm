module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
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
    = FetchedAndChecked Version Zip (Result CheckError (List Review.Rule.ReviewError))
    | FetchingZipFailed Version Http.Error


type CheckError
    = ElmJsonMissing
    | ElmJsonIsForApplication


packageVersion : PackageStatus -> Version
packageVersion packageStatus =
    case packageStatus of
        FetchedAndChecked version _ _ ->
            version

        FetchingZipFailed version _ ->
            version


packageZip : PackageStatus -> Maybe Zip
packageZip packageStatus =
    case packageStatus of
        FetchedAndChecked _ zip _ ->
            Just zip

        FetchingZipFailed _ _ ->
            Nothing


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
    | FetchedZipResult String Version (Result Http.Error Zip)


type ToFrontend
    = NoOpToFrontend


type alias PackageEndpoint =
    { url : String, hash : String }
