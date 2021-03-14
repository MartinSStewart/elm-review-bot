module Backend exposing (..)

import Dict exposing (Dict)
import Elm.Constraint exposing (Constraint)
import Elm.Docs
import Elm.Package
import Elm.Project
import Http
import Json.Decode exposing (Decoder)
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import NoUnused.Dependencies
import Process
import Review.Project
import Review.Project.Dependency
import Review.Rule
import Task exposing (Task)
import Types exposing (..)
import Version exposing (MajorVersion, Version)
import Zip exposing (Zip)
import Zip.Entry


type alias Model =
    BackendModel


decodeAllPackages : Decoder (List ( String, Version ))
decodeAllPackages =
    Json.Decode.list
        (Json.Decode.string
            |> Json.Decode.andThen
                (\text ->
                    case String.split "@" text of
                        name :: versionText :: [] ->
                            case String.split "." versionText of
                                major :: minor :: patch :: [] ->
                                    Maybe.map3 Version
                                        (String.toInt major)
                                        (String.toInt minor)
                                        (String.toInt patch)
                                        |> Maybe.map (Tuple.pair name >> Json.Decode.succeed)
                                        |> Maybe.withDefault (Json.Decode.fail "Invalid version number")

                                _ ->
                                    Json.Decode.fail "Invalid version number"

                        _ ->
                            Json.Decode.fail "Invalid author/name@version"
                )
        )
        |> Json.Decode.map List.reverse


getAllPackages : Int -> Cmd BackendMsg
getAllPackages cachedCount =
    Http.get
        { url = "https://package.elm-lang.org/all-packages/since/" ++ String.fromInt cachedCount
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
                ++ Version.toString version
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
    ( { cachedPackages = Dict.empty }
    , getAllPackages packageCountOffset
    )


packageCountOffset =
    6557


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        GotNewPackagePreviews result ->
            case Debug.log "result" result of
                Ok newPackages ->
                    let
                        majorOnly : Dict ( String, MajorVersion ) Version
                        majorOnly =
                            List.gatherEqualsBy Tuple.first newPackages
                                |> List.map (\( ( name, version ), rest ) -> ( name, version :: List.map Tuple.second rest ))
                                |> List.concatMap
                                    (\( packageName, versions ) ->
                                        List.reverse versions
                                            |> List.uniqueBy .major
                                            |> List.map (\version -> ( ( packageName, version.major ), version ))
                                    )
                    in
                    ( { model
                        | --{ cachedPackages =
                          --       Dict.union
                          --           (Dict.map (\_ version -> FetchingZip version) majorOnly)
                          --           model.cachedPackages
                          todos = newPackages
                      }
                    , Cmd.none
                      --, Dict.toList majorOnly
                      --    |> List.indexedMap
                      --        (\index ( ( packageName, _ ), version ) ->
                      --            Process.sleep (toFloat (index * 2000))
                      --                |> Task.andThen (\() -> getPackageEndpoint packageName version)
                      --                |> Task.andThen getPackageZip
                      --                |> Task.attempt (FetchedZipResult packageName version)
                      --        )
                      --    |> Cmd.batch
                    )

                Err _ ->
                    ( model, Cmd.none )

        FetchedZipResult packageName version result ->
            --let
            --    _ =
            --        Debug.log "model " model.cachedPackages
            --in
            ( { cachedPackages =
                    Dict.update
                        ( packageName, version.major )
                        (Maybe.map
                            (\value ->
                                case value of
                                    FetchingZip version_ ->
                                        if version_ == version then
                                            case result of
                                                Ok zip ->
                                                    FetchedAndChecked version zip (checkPackage model.cachedPackages zip)

                                                Err error ->
                                                    FetchingZipFailed version error

                                        else
                                            FetchingZip version_

                                    _ ->
                                        value
                            )
                        )
                        model.cachedPackages
              }
            , Cmd.none
            )


checkPackage : Dict ( String, Int ) PackageStatus -> Zip -> Result CheckError (List Review.Rule.ReviewError)
checkPackage cached zip =
    let
        project : Review.Project.Project
        project =
            List.foldl
                (\zipEntry project_ ->
                    if Zip.Entry.isDirectory zipEntry then
                        project_

                    else
                        case ( String.split "/" (Zip.Entry.path zipEntry), Zip.Entry.toString zipEntry ) of
                            ( "src" :: next :: restOfPath, Ok source ) ->
                                let
                                    ( last, middle ) =
                                        List.unconsLast (next :: restOfPath) |> Maybe.withDefault ( next, [] )

                                    moduleName =
                                        String.split "." last |> List.head |> Maybe.withDefault ""
                                in
                                Review.Project.addModule
                                    { path = String.join "." middle ++ "." ++ moduleName
                                    , source = source
                                    }
                                    project_

                            ( path, Ok source ) ->
                                if List.last path |> (==) (Just "elm.json") then
                                    case Json.Decode.decodeString Elm.Project.decoder source of
                                        Ok elmJsonProject ->
                                            Review.Project.addElmJson
                                                { path = Zip.Entry.path zipEntry, raw = source, project = elmJsonProject }
                                                project_

                                        Err _ ->
                                            project_

                                else
                                    project_

                            _ ->
                                project_
                )
                Review.Project.new
                (Zip.ls zip)
    in
    case Review.Project.elmJson project of
        Just elmJson ->
            case elmJson.project of
                Elm.Project.Package packageInfo ->
                    let
                        projectWithDependencies : Review.Project.Project
                        projectWithDependencies =
                            List.foldl
                                (\( packageName, constraint ) state ->
                                    case
                                        Dict.get
                                            ( Debug.log "packagename" (Elm.Package.toString packageName)
                                            , majorVersion constraint
                                            )
                                            cached
                                    of
                                        Just package ->
                                            case Types.packageZip package of
                                                Just zip_ ->
                                                    addDependency zip_ state

                                                Nothing ->
                                                    state

                                        Nothing ->
                                            state
                                )
                                project
                                packageInfo.deps
                    in
                    Review.Rule.reviewV2 [ NoUnused.Dependencies.rule ] Nothing projectWithDependencies
                        |> .errors
                        |> Debug.log ("errors " ++ elmJson.path)
                        |> Ok

                Elm.Project.Application _ ->
                    Err ElmJsonIsForApplication

        Nothing ->
            Err ElmJsonMissing


addDependency : Zip -> Review.Project.Project -> Review.Project.Project
addDependency zip project =
    let
        data =
            List.foldl
                (\zipEntry data_ ->
                    if Zip.Entry.isDirectory zipEntry then
                        data_

                    else
                        case ( String.split "/" (Zip.Entry.path zipEntry), Zip.Entry.toString zipEntry ) of
                            ( path, Ok source ) ->
                                if List.last path |> (==) (Just "elm.json") then
                                    case Json.Decode.decodeString Elm.Project.decoder source of
                                        Ok elmJsonProject ->
                                            { data_ | elmJson = Just elmJsonProject }

                                        Err _ ->
                                            data_

                                else if List.last path |> (==) (Just "docs.json") then
                                    case Json.Decode.decodeString Elm.Docs.decoder source of
                                        Ok elmJsonProject ->
                                            { data_ | modules = elmJsonProject :: data_.modules }

                                        Err _ ->
                                            data_

                                else
                                    data_

                            _ ->
                                data_
                )
                { name = Nothing, elmJson = Nothing, modules = [] }
                (Zip.ls zip)
    in
    case
        Maybe.map2
            (\name elmJson -> Review.Project.Dependency.create name elmJson data.modules)
            data.name
            data.elmJson
            |> Debug.log "dependency"
    of
        Just dependency ->
            Review.Project.addDependency dependency project

        Nothing ->
            project


majorVersion : Constraint -> Int
majorVersion constraint =
    Elm.Constraint.toString constraint
        |> String.split " "
        |> List.head
        |> Maybe.andThen String.toInt
        |> Maybe.withDefault 1


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )
