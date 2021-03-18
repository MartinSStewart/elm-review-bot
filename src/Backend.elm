module Backend exposing (..)

import Dict exposing (Dict)
import Elm.Constraint exposing (Constraint)
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version as Version exposing (Version)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import List.Nonempty
import NoUnused.Dependencies
import Review.Project
import Review.Project.Dependency
import Review.Rule
import Set
import Task exposing (Task)
import Types exposing (..)
import Zip exposing (Zip)
import Zip.Entry


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
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Lamdera.onConnect ClientConnected
                    , Lamdera.onDisconnect ClientDisconnected
                    ]
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { cachedPackages = Dict.empty, todos = [], clients = Set.empty }
    , getAllPackages packageCountOffset
    )


packageCountOffset =
    6557


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        GotNewPackagePreviews result ->
            case result of
                Ok newPackages ->
                    let
                        ( todosLeft, cmd ) =
                            nextTodo
                                (Dict.values model.cachedPackages |> List.map List.length |> List.sum)
                                (model.todos ++ newPackages)
                    in
                    ( { model | todos = todosLeft }, cmd )

                Err _ ->
                    ( model, Cmd.none )

        FetchedZipResult packageName version index result ->
            let
                ( todosLeft, cmd ) =
                    nextTodo (Dict.size model.cachedPackages) model.todos

                packageStatus =
                    case result of
                        Ok ( zip, docs, Elm.Project.Package elmJson ) ->
                            FetchedAndChecked
                                { version = version
                                , index = index
                                , docs = docs
                                , elmJson = elmJson
                                , errors = checkPackage elmJson model.cachedPackages zip
                                }

                        Ok ( _, _, Elm.Project.Application _ ) ->
                            FetchingZipFailed version index (Http.BadBody "Invalid elm.json type")

                        Err error ->
                            FetchingZipFailed version index error
            in
            ( { model
                | cachedPackages =
                    Dict.update
                        packageName
                        (\maybeValue -> packageStatus :: Maybe.withDefault [] maybeValue |> Just)
                        model.cachedPackages
                , todos = todosLeft
              }
            , cmd
                :: List.map
                    (\client ->
                        Dict.singleton packageName [ Types.statusToStatusFrontend packageStatus ]
                            |> Updates
                            |> Lamdera.sendToFrontend client
                    )
                    (Set.toList model.clients)
                |> Cmd.batch
            )

        ClientConnected _ clientId ->
            ( { model | clients = Set.insert clientId model.clients }
            , Dict.map (\_ value -> List.map statusToStatusFrontend value) model.cachedPackages
                |> Updates
                |> Lamdera.sendToFrontend clientId
            )

        ClientDisconnected _ clientId ->
            ( { model | clients = Set.remove clientId model.clients }, Cmd.none )


nextTodo : Int -> List ( String, Version ) -> ( List ( String, Version ), Cmd BackendMsg )
nextTodo count todos =
    case todos of
        ( packageName, version ) :: rest ->
            ( rest
            , getPackageEndpoint packageName version
                |> Task.andThen getPackageZip
                |> Task.andThen (\zip -> getPackageDocs packageName version |> Task.map (Tuple.pair zip))
                |> Task.andThen (\( zip, docs ) -> getPackageElmJson packageName version |> Task.map (\elmJson -> ( zip, docs, elmJson )))
                |> Task.attempt (FetchedZipResult packageName version count)
            )

        [] ->
            ( todos, Cmd.none )


project : Elm.Project.PackageInfo -> Zip -> Review.Project.Project
project elmJson zip =
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
                        if
                            String.endsWith ".elm" (List.Nonempty.last nonemptyPath)
                                && (List.Nonempty.get 1 nonemptyPath == "src" || List.Nonempty.get 1 nonemptyPath == "tests")
                        then
                            Review.Project.addModule
                                { path = Zip.Entry.path zipEntry
                                , source = source
                                }
                                project_

                        else
                            project_

                    _ ->
                        project_
        )
        Review.Project.new
        (Zip.ls zip)
        |> Review.Project.addElmJson
            { path = "src/elm.json"
            , raw = Elm.Project.Package elmJson |> Elm.Project.encode |> Json.Encode.encode 0
            , project = Elm.Project.Package elmJson
            }


checkPackage : Elm.Project.PackageInfo -> Dict String (List PackageStatus) -> Zip -> List Review.Rule.ReviewError
checkPackage elmJson cached zip =
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

                                                FetchingZipFailed _ _ _ ->
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
                (project elmJson zip)
                elmJson.deps
    in
    case Version.fromTuple ( 0, 19, 1 ) of
        Just version ->
            if Elm.Constraint.check version elmJson.elm then
                Review.Rule.reviewV2 [ NoUnused.Dependencies.rule ] Nothing projectWithDependencies
                    |> .errors

            else
                []

        Nothing ->
            []


addDependency : Elm.Project.PackageInfo -> List Elm.Docs.Module -> Review.Project.Project -> Review.Project.Project
addDependency elmJson docs =
    Review.Project.addDependency
        (Review.Project.Dependency.create (Elm.Package.toString elmJson.name) (Elm.Project.Package elmJson) docs)


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )
