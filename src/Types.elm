module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.Range exposing (Range)
import Elm.Version exposing (Version)
import Http
import Lamdera exposing (ClientId, SessionId)
import Review.Fix exposing (Fix)
import Review.Rule
import Set exposing (Set)
import Url exposing (Url)
import Zip exposing (Zip)


type alias FrontendModel =
    { state : Dict String (List PackageStatusFrontend)
    , key : Key
    , order : DisplayOrder
    }


type alias Error =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    , fixes : Maybe (List Fix)
    }


type DisplayOrder
    = RequestOrder
    | Alphabetical


type PackageStatus
    = FetchedAndChecked
        { version : Version
        , index : Int
        , docs : List Elm.Docs.Module
        , elmJson : Elm.Project.PackageInfo
        , errors : List Error
        }
    | FetchingZipFailed Version Int Http.Error


type PackageStatusFrontend
    = FetchedAndChecked_
        { version : Version
        , index : Int
        , errors : List Error
        }
    | FetchingZipFailed_ Version Int Http.Error


statusToStatusFrontend : PackageStatus -> PackageStatusFrontend
statusToStatusFrontend packageStatus =
    case packageStatus of
        FetchedAndChecked { version, index, errors } ->
            FetchedAndChecked_
                { version = version
                , index = index
                , errors = errors
                }

        FetchingZipFailed version index error ->
            FetchingZipFailed_ version index error


packageVersion : PackageStatus -> Version
packageVersion packageStatus =
    case packageStatus of
        FetchedAndChecked { version } ->
            version

        FetchingZipFailed version _ _ ->
            version


packageVersion_ : PackageStatusFrontend -> Version
packageVersion_ packageStatus =
    case packageStatus of
        FetchedAndChecked_ { version } ->
            version

        FetchingZipFailed_ version _ _ ->
            version


packageIndex : PackageStatusFrontend -> Int
packageIndex packageStatus =
    case packageStatus of
        FetchedAndChecked_ { index } ->
            index

        FetchingZipFailed_ _ index _ ->
            index


type alias BackendModel =
    { cachedPackages : Dict String (List PackageStatus)
    , todos : List ( String, Version )
    , clients : Set String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ToggleOrder
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = GotNewPackagePreviews (Result Http.Error (List ( String, Version )))
    | FetchedZipResult String Version Int (Result Http.Error ( Zip, List Elm.Docs.Module, Elm.Project.Project ))
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = Updates (Dict String (List PackageStatusFrontend))


type alias PackageEndpoint =
    { url : String, hash : String }
