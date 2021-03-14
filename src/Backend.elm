module Backend exposing (..)

import Dict exposing (Dict)
import Html
import Http
import Json.Decode exposing (Decoder)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Task exposing (Task)
import Types exposing (..)
import Zip exposing (Zip)


type alias Model =
    BackendModel


decodePackages : Decoder (Dict String { version : Version })
decodePackages =
    Json.Decode.map2 (\name version -> ( name, { version = version } ))
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "version" decodeVersion)
        |> Json.Decode.list
        |> Json.Decode.map Dict.fromList


decodeAllPackages : Decoder (Dict String (List Version))
decodeAllPackages =
    Json.Decode.dict (Json.Decode.list decodeVersion)


decodeVersion : Decoder Version
decodeVersion =
    Json.Decode.string
        |> Json.Decode.andThen
            (\text ->
                case String.split "." text of
                    major :: minor :: patch :: [] ->
                        Maybe.map3 (\major_ minor_ patch_ -> Version major_ minor_ patch_ |> Json.Decode.succeed)
                            (String.toInt major)
                            (String.toInt minor)
                            (String.toInt patch)
                            |> Maybe.withDefault (Json.Decode.fail "Invalid version number")

                    _ ->
                        Json.Decode.fail "Invalid version number"
            )


getAllPackages : Maybe Int -> Cmd BackendMsg
getAllPackages cachedCount =
    Http.get
        { url =
            case cachedCount of
                Just count ->
                    "https://package.elm-lang.org/all-packages/since/" ++ String.fromInt count

                Nothing ->
                    "https://package.elm-lang.org/all-packages"
        , expect = Http.expectJson GotNewPackagePreviews decodeAllPackages
        }


decodePackageEndpoint : Decoder PackageEndpoint
decodePackageEndpoint =
    Json.Decode.map2 PackageEndpoint
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "hash" Json.Decode.string)


getPackageEndpoint : String -> Version -> Task Http.Error PackageEndpoint
getPackageEndpoint packageName version =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            "https://package.elm-lang.org/packages/"
                ++ packageName
                ++ "/"
                ++ Types.versionToString version
                ++ "/endpoint.json"
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url ->
                            Http.BadUrl url |> Err

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata body ->
                            Http.BadStatus metadata.statusCode |> Err

                        Http.GoodStatus_ metadata body ->
                            case Json.Decode.decodeString decodePackageEndpoint body of
                                Ok ok ->
                                    Ok ok

                                Err error ->
                                    Http.BadBody (Json.Decode.errorToString error) |> Err
                )
        , timeout = Nothing
        }


getPackageZip : PackageEndpoint -> Task Http.Error Zip
getPackageZip packageEndpoint =
    Http.task
        { method = "GET"
        , headers = []
        , url = packageEndpoint.url
        , body = Http.emptyBody
        , resolver =
            Http.bytesResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url ->
                            Http.BadUrl url |> Err

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata body ->
                            Http.BadStatus metadata.statusCode |> Err

                        Http.GoodStatus_ metadata body ->
                            Zip.fromBytes body |> Result.fromMaybe (Http.BadBody "Failed to open zip file.")
                )
        , timeout = Nothing
        }


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { cachedPackages = Dict.empty, cachedCount = 0 }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotNewPackagePreviews result ->
            case result of
                Ok newPackages ->
                    ( { cachedPackages = model.cachedPackages
                      , cachedCount =
                            Dict.foldl
                                (\_ value counter -> List.length value + counter)
                                0
                                newPackages
                      }
                    , Cmd.none
                    )

                Err error ->
                    Debug.todo ""


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )
