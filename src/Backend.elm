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
import List.Nonempty
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

                        Http.BadStatus_ metadata _ ->
                            Http.BadStatus metadata.statusCode |> Err

                        Http.GoodStatus_ _ body ->
                            case Json.Decode.decodeString decodePackageEndpoint body of
                                Ok ok ->
                                    Ok ok

                                Err error ->
                                    Http.BadBody (Json.Decode.errorToString error) |> Err
                )
        , timeout = Nothing
        }


getPackageDocs : String -> Version -> Task Http.Error (List Elm.Docs.Module)
getPackageDocs packageName version =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            "https://package.elm-lang.org/packages/"
                ++ packageName
                ++ "/"
                ++ Version.toString version
                ++ "/docs.json"
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

                        Http.BadStatus_ metadata _ ->
                            Http.BadStatus metadata.statusCode |> Err

                        Http.GoodStatus_ _ body ->
                            case Json.Decode.decodeString (Json.Decode.list Elm.Docs.decoder) body of
                                Ok ok ->
                                    Ok ok

                                Err error ->
                                    Http.BadBody (Json.Decode.errorToString error) |> Err
                )
        , timeout = Nothing
        }


getPackageElmJson : String -> Version -> Task Http.Error Elm.Project.Project
getPackageElmJson packageName version =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            "https://package.elm-lang.org/packages/"
                ++ packageName
                ++ "/"
                ++ Version.toString version
                ++ "/elm.json"
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

                        Http.BadStatus_ metadata _ ->
                            Http.BadStatus metadata.statusCode |> Err

                        Http.GoodStatus_ _ body ->
                            case Json.Decode.decodeString Elm.Project.decoder body of
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

                        Http.BadStatus_ metadata _ ->
                            Http.BadStatus metadata.statusCode |> Err

                        Http.GoodStatus_ _ body ->
                            Zip.fromBytes body |> Result.fromMaybe (Http.BadBody "Failed to open zip file.")
                )
        , timeout = Nothing
        }


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
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
                    ( { model | todos = todosLeft }, cmd )

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
                            (case result of
                                Ok ( zip, docs, Elm.Project.Package elmJson ) ->
                                    FetchedAndChecked
                                        { version = version
                                        , docs = docs
                                        , elmJson = elmJson
                                        , result = checkPackage model.cachedPackages zip
                                        }

                                Ok ( _, _, Elm.Project.Application _ ) ->
                                    FetchingZipFailed version (Http.BadBody "Invalid elm.json type")

                                Err error ->
                                    FetchingZipFailed version error
                            )
                                :: Maybe.withDefault [] maybeValue
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
                |> Task.andThen (\zip -> getPackageDocs packageName version |> Task.map (Tuple.pair zip))
                |> Task.andThen (\( zip, docs ) -> getPackageElmJson packageName version |> Task.map (\elmJson -> ( zip, docs, elmJson )))
                |> Task.attempt (FetchedZipResult packageName version)
            )

        [] ->
            ( todos, Cmd.none )


project : Zip -> Review.Project.Project
project zip =
    List.foldl
        (\zipEntry project_ ->
            if Zip.Entry.isDirectory zipEntry then
                project_

            else
                case
                    ( String.split "/" (Zip.Entry.path zipEntry) |> List.Nonempty.fromList
                    , Zip.Entry.toString zipEntry
                    )
                of
                    ( Just nonemptyPath, Ok source ) ->
                        if List.Nonempty.last nonemptyPath == "elm.json" then
                            case Json.Decode.decodeString Elm.Project.decoder source of
                                Ok elmJsonProject ->
                                    Review.Project.addElmJson
                                        { path = Zip.Entry.path zipEntry |> Debug.log "path ", raw = source, project = elmJsonProject }
                                        project_

                                Err _ ->
                                    project_

                        else if List.Nonempty.last nonemptyPath |> String.endsWith ".elm" |> (==) True then
                            Review.Project.addModule
                                { path = Zip.Entry.path zipEntry
                                , source = source
                                }
                                project_

                        else
                            let
                                _ =
                                    Debug.log "project2_" nonemptyPath
                            in
                            project_

                    _ ->
                        Debug.log "project_" project_
        )
        Review.Project.new
        (Zip.ls zip)


checkPackage : Dict String (List PackageStatus) -> Zip -> Result CheckError (List Review.Rule.ReviewError)
checkPackage cached zip =
    let
        project_ =
            project zip
    in
    case Review.Project.elmJson project_ of
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
                                                        if Elm.Constraint.check (Types.packageVersion package) constraint then
                                                            case package of
                                                                FetchedAndChecked data ->
                                                                    Just
                                                                        ( Types.packageVersion package
                                                                        , ( data.elmJson, data.docs )
                                                                        )

                                                                FetchingZipFailed _ _ ->
                                                                    Nothing

                                                        else
                                                            Nothing
                                                    )
                                                    packages
                                                    |> List.maximumWith (\( a, _ ) ( b, _ ) -> Version.compare a b)
                                            of
                                                Just ( _, ( elmJson_, docs_ ) ) ->
                                                    addDependency elmJson_ docs_ state

                                                Nothing ->
                                                    state

                                        Nothing ->
                                            state
                                )
                                project_
                                packageInfo.deps
                    in
                    Review.Rule.reviewV2
                        [ NoUnused.Dependencies.rule ]
                        Nothing
                        (Debug.log "project" projectWithDependencies)
                        |> .errors
                        |> Debug.log ("errors " ++ elmJson.path ++ "   ")
                        |> Ok

                Elm.Project.Application _ ->
                    Err ElmJsonIsForApplication

        Nothing ->
            Err ElmJsonMissing


addDependency : Elm.Project.PackageInfo -> List Elm.Docs.Module -> Review.Project.Project -> Review.Project.Project
addDependency elmJson docs =
    Review.Project.addDependency
        (Review.Project.Dependency.create (Elm.Package.toString elmJson.name) (Elm.Project.Package elmJson) docs)


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )
