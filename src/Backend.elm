module Backend exposing (..)

import Dict exposing (Dict)
import Elm.Constraint exposing (Constraint)
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version as Version exposing (Version)
import Http
import Json.Decode exposing (Decoder)
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import NoUnused.Dependencies
import Review.Project
import Review.Project.Dependency
import Review.Rule
import Task exposing (Task)
import Types exposing (..)
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
                            case Version.fromString versionText of
                                Just version ->
                                    Json.Decode.succeed ( name, version )

                                Nothing ->
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
    ( { cachedPackages = Dict.empty, todos = [] }
    , getAllPackages packageCountOffset
    )


packageCountOffset =
    6557


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        GotNewPackagePreviews result ->
            case result of
                Ok newPackages ->
                    let
                        ( todosLeft, cmd ) =
                            nextTodo (model.todos ++ newPackages)
                    in
                    --let
                    --    majorOnly : Dict ( String, MajorVersion ) Version
                    --    majorOnly =
                    --        List.gatherEqualsBy Tuple.first newPackages
                    --            |> List.map (\( ( name, version ), rest ) -> ( name, version :: List.map Tuple.second rest ))
                    --            |> List.concatMap
                    --                (\( packageName, versions ) ->
                    --                    List.reverse versions
                    --                        |> List.uniqueBy .major
                    --                        |> List.map (\version -> ( ( packageName, version.major ), version ))
                    --                )
                    --in
                    ( { model
                        | --{ cachedPackages =
                          --       Dict.union
                          --           (Dict.map (\_ version -> FetchingZip version) majorOnly)
                          --           model.cachedPackages
                          todos = todosLeft
                      }
                    , cmd
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
            let
                ( todosLeft, cmd ) =
                    nextTodo model.todos
            in
            ( { cachedPackages =
                    Dict.update
                        packageName
                        (\maybeValue ->
                            let
                                value =
                                    Maybe.withDefault [] maybeValue
                            in
                            (case result of
                                Ok zip ->
                                    FetchedAndChecked version zip (checkPackage model.cachedPackages zip) :: value

                                Err error ->
                                    FetchingZipFailed version error :: value
                            )
                                |> Just
                        )
                        model.cachedPackages
              , todos = todosLeft
              }
            , cmd
            )


nextTodo : List ( String, Version ) -> ( List ( String, Version ), Cmd BackendMsg )
nextTodo todos =
    case todos of
        ( packageName, version ) :: rest ->
            ( rest
            , getPackageEndpoint packageName version
                |> Task.andThen getPackageZip
                |> Task.attempt (FetchedZipResult packageName version)
            )

        [] ->
            ( todos, Cmd.none )


checkPackage : Dict String (List PackageStatus) -> Zip -> Result CheckError (List Review.Rule.ReviewError)
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
                                                { path = Zip.Entry.path zipEntry |> Debug.log "path ", raw = source, project = elmJsonProject }
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
                                    case Dict.get (Elm.Package.toString packageName) cached of
                                        Just packages ->
                                            case
                                                List.filterMap
                                                    (\package ->
                                                        if
                                                            Elm.Constraint.check
                                                                (Types.packageVersion package)
                                                                constraint
                                                        then
                                                            Maybe.map
                                                                (Tuple.pair (Types.packageVersion package))
                                                                (Types.packageZip package)

                                                        else
                                                            Nothing
                                                    )
                                                    packages
                                                    |> List.maximumWith (\( a, _ ) ( b, _ ) -> Version.compare a b)
                                            of
                                                Just ( _, zip_ ) ->
                                                    addDependency zip_ state

                                                Nothing ->
                                                    state

                                        Nothing ->
                                            state
                                )
                                project
                                packageInfo.deps
                    in
                    Review.Rule.reviewV2 [ NoUnused.Dependencies.rule ] Nothing (Debug.log "project" projectWithDependencies)
                        |> .errors
                        |> Debug.log ("errors " ++ elmJson.path ++ "   ")
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
                                        Ok (Elm.Project.Package elmJsonProject) ->
                                            { data_
                                                | elmJson = Just (Elm.Project.Package elmJsonProject)
                                                , name = Elm.Package.toString elmJsonProject.name |> Just
                                            }

                                        _ ->
                                            data_

                                else if List.last path |> (==) (Just "docs.json") then
                                    case Json.Decode.decodeString (Json.Decode.list Elm.Docs.decoder) source of
                                        Ok elmJsonProjects ->
                                            { data_ | modules = elmJsonProjects ++ data_.modules }

                                        Err _ ->
                                            data_

                                else
                                    data_

                            _ ->
                                data_
                )
                { name = Nothing, elmJson = Nothing, modules = [] }
                (Zip.ls zip)
                |> Debug.log "dependencyData"
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
