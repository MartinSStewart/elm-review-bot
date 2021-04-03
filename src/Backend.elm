module Backend exposing (..)

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Elm.Constraint exposing (Constraint)
import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Parser
import Elm.Project
import Elm.Version as Version exposing (Version)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import List.Nonempty
import NoUnused.Dependencies
import Parser exposing (Parser)
import Review.Project
import Review.Project.Dependency
import Review.Rule
import Set exposing (Set)
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


getPackageZip : PackageEndpoint -> Task Http.Error Bytes
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
                            Ok body
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
    ( { cachedPackages = Dict.empty, clients = Set.empty, updateIndex = 0 }
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
                        model2 =
                            List.foldl
                                (\( packageName, version ) state ->
                                    { state
                                        | cachedPackages =
                                            Dict.update
                                                packageName
                                                (\maybeValue ->
                                                    Pending version state.updateIndex
                                                        :: Maybe.withDefault [] maybeValue
                                                        |> Just
                                                )
                                                state.cachedPackages
                                        , updateIndex = state.updateIndex + 1
                                    }
                                )
                                model
                                newPackages
                    in
                    ( model2
                    , nextTodo model2
                    )

                Err _ ->
                    ( model, Cmd.none )

        FetchedElmJsonAndDocs { packageName, version } result ->
            let
                packageStatus =
                    case result of
                        Ok ( Elm.Project.Package elmJson, docs ) ->
                            Fetched
                                { version = version
                                , updateIndex = model.updateIndex
                                , docs = docs
                                , elmJson = elmJson
                                }

                        Ok ( Elm.Project.Application _, _ ) ->
                            FetchingElmJsonAndDocsFailed
                                version
                                model.updateIndex
                                (Http.BadBody "Invalid elm.json package type")

                        Err error ->
                            FetchingElmJsonAndDocsFailed version model.updateIndex error

                model2 =
                    { model
                        | cachedPackages =
                            Dict.update
                                packageName
                                (Maybe.map (List.setIf (Types.packageVersion >> (==) version) packageStatus))
                                model.cachedPackages
                        , updateIndex = model.updateIndex + 1
                    }
            in
            ( model2
            , Cmd.batch
                (nextTodo model2
                    :: List.map
                        (\client ->
                            Lamdera.sendToFrontend
                                client
                                (Updates
                                    (Dict.singleton
                                        packageName
                                        (List.filterMap Types.statusToStatusFrontend [ packageStatus ])
                                    )
                                )
                        )
                        (Set.toList model2.clients)
                )
            )

        FetchedZipResult { packageName, version } elmJson docs result ->
            let
                packageStatus =
                    case ( result, elmJson ) of
                        ( Ok zipBytes, elmJson_ ) ->
                            case Zip.fromBytes zipBytes of
                                Just zip ->
                                    FetchedAndChecked
                                        { version = version
                                        , updateIndex = model.updateIndex
                                        , docs = docs
                                        , elmJson = elmJson_
                                        , errors = checkPackage elmJson_ model.cachedPackages zip
                                        }

                                Nothing ->
                                    FetchingZipFailed version model.updateIndex (Http.BadBody "Invalid zip")

                        ( Err error, _ ) ->
                            FetchingZipFailed version model.updateIndex error

                model2 =
                    { model
                        | cachedPackages =
                            Dict.update
                                packageName
                                (Maybe.map (List.setIf (Types.packageVersion >> (==) version) packageStatus))
                                model.cachedPackages
                        , updateIndex = model.updateIndex + 1
                    }
            in
            ( model2
            , Cmd.batch
                (nextTodo model2
                    :: List.map
                        (\client ->
                            Lamdera.sendToFrontend
                                client
                                (Updates
                                    (Dict.singleton
                                        packageName
                                        (List.filterMap Types.statusToStatusFrontend [ packageStatus ])
                                    )
                                )
                        )
                        (Set.toList model2.clients)
                )
            )

        ClientConnected _ clientId ->
            ( { model | clients = Set.insert clientId model.clients }
            , Dict.map (\_ value -> List.filterMap statusToStatusFrontend value) model.cachedPackages
                |> Updates
                |> Lamdera.sendToFrontend clientId
            )

        ClientDisconnected _ clientId ->
            ( { model | clients = Set.remove clientId model.clients }, Cmd.none )


nextTodo : BackendModel -> Cmd BackendMsg
nextTodo model =
    let
        maybeNextTodo : Maybe ( String, PackageStatus )
        maybeNextTodo =
            Dict.foldl
                (\packageName versions currentTodo ->
                    List.foldl
                        (\packageStatus currentTodo_ ->
                            case packageStatus of
                                Pending _ _ ->
                                    Just ( packageName, packageStatus )

                                Fetched { version } ->
                                    case currentTodo_ of
                                        Just ( _, Pending _ _ ) ->
                                            currentTodo_

                                        _ ->
                                            if
                                                List.count
                                                    (Types.packageVersion >> Version.compare version >> (/=) GT)
                                                    versions
                                                    == 1
                                            then
                                                Just ( packageName, packageStatus )

                                            else
                                                currentTodo_

                                FetchedAndChecked _ ->
                                    currentTodo_

                                FetchingZipFailed _ _ _ ->
                                    currentTodo_

                                FetchingElmJsonAndDocsFailed _ _ _ ->
                                    currentTodo_
                        )
                        currentTodo
                        versions
                )
                Nothing
                model.cachedPackages
    in
    case maybeNextTodo of
        Just ( packageName, Pending version _ ) ->
            getPackageElmJson packageName version
                |> Task.andThen (\elmJson -> getPackageDocs packageName version |> Task.map (Tuple.pair elmJson))
                |> Task.attempt (FetchedElmJsonAndDocs { packageName = packageName, version = version })

        Just ( packageName, Fetched { version, elmJson, docs } ) ->
            getPackageEndpoint packageName version
                |> Task.andThen (\packageEndpoint -> getPackageZip packageEndpoint)
                |> Task.attempt (FetchedZipResult { packageName = packageName, version = version } elmJson docs)

        Just ( _, FetchedAndChecked _ ) ->
            Cmd.none

        Just ( _, FetchingZipFailed _ _ _ ) ->
            Cmd.none

        Just ( _, FetchingElmJsonAndDocsFailed _ _ _ ) ->
            Cmd.none

        Nothing ->
            Cmd.none


project :
    Elm.Project.PackageInfo
    -> List { path : String, source : String }
    -> List { path : String, source : String }
    -> Review.Project.Project
project elmJson srcModules testModules =
    let
        importParser : Parser String
        importParser =
            Parser.chompWhile (\c -> c /= ' ' && c /= '\t' && c /= '\n') |> Parser.getChompedString

        srcModules_ : Dict String { path : String, source : String, imports : List String }
        srcModules_ =
            srcModules
                |> List.map
                    (\{ path, source } ->
                        let
                            moduleName =
                                String.dropLeft (String.length "src/") path
                                    |> String.dropRight (String.length ".elm")
                                    |> String.replace "/" "."
                        in
                        ( moduleName
                        , { path = path
                          , source = source
                          , imports =
                                String.filter ((/=) '\u{000D}') source
                                    |> String.indexes "\nimport"
                                    |> List.filterMap
                                        (\index ->
                                            String.slice (index + 7) 200 source
                                                |> String.trim
                                                |> Parser.run importParser
                                                |> Result.toMaybe
                                        )
                          }
                        )
                    )
                |> Dict.fromList

        directlyExposedModules =
            (case elmJson.exposed of
                Elm.Project.ExposedList exposed ->
                    exposed

                Elm.Project.ExposedDict exposed ->
                    List.concatMap Tuple.second exposed
            )
                |> List.filterMap
                    (\import_ ->
                        Dict.get (Elm.Module.toString import_) srcModules_
                            |> Maybe.map (Tuple.pair (Elm.Module.toString import_))
                    )
                |> Dict.fromList

        usedSrcModules unchecked collected =
            case Dict.toList unchecked of
                ( moduleName, head ) :: rest ->
                    let
                        collected_ =
                            Dict.insert moduleName head collected
                    in
                    usedSrcModules
                        (List.filterMap
                            (\import_ ->
                                case ( Dict.get import_ srcModules_, Dict.member import_ collected_ ) of
                                    ( Just module_, False ) ->
                                        Just ( import_, module_ )

                                    _ ->
                                        Nothing
                            )
                            head.imports
                            ++ rest
                            |> Dict.fromList
                        )
                        collected_

                [] ->
                    collected
    in
    usedSrcModules directlyExposedModules Dict.empty
        |> Dict.toList
        |> List.map (\( _, module_ ) -> { path = module_.path, source = module_.source })
        |> (++) testModules
        |> List.foldl Review.Project.addModule Review.Project.new
        |> Debug.log ""
        |> Review.Project.addElmJson
            { path = "src/elm.json"
            , raw = Elm.Project.Package elmJson |> Elm.Project.encode |> Json.Encode.encode 0
            , project = Elm.Project.Package elmJson
            }


checkPackage : Elm.Project.PackageInfo -> Dict String (List PackageStatus) -> Zip -> List Error
checkPackage elmJson cached zip =
    let
        modules : String -> List { path : String, source : String }
        modules folderName =
            List.filterMap
                (\zipEntry ->
                    if Zip.Entry.isDirectory zipEntry then
                        Nothing

                    else
                        case
                            ( String.split "/" (Zip.Entry.path zipEntry) |> List.Nonempty.fromList
                            , Zip.Entry.toString zipEntry
                            )
                        of
                            ( Just nonemptyPath, Ok source ) ->
                                if
                                    String.endsWith ".elm" (List.Nonempty.last nonemptyPath)
                                        && (List.Nonempty.get 1 nonemptyPath == folderName)
                                then
                                    Just
                                        { path = Zip.Entry.path zipEntry
                                        , source = source
                                        }

                                else
                                    Nothing

                            _ ->
                                Nothing
                )
                (Zip.ls zip)

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
                                                Pending _ _ ->
                                                    Nothing

                                                Fetched data ->
                                                    Just
                                                        ( Types.packageVersion package
                                                        , ( data.elmJson, data.docs )
                                                        )

                                                FetchedAndChecked data ->
                                                    Just
                                                        ( Types.packageVersion package
                                                        , ( data.elmJson, data.docs )
                                                        )

                                                FetchingZipFailed _ _ _ ->
                                                    Nothing

                                                FetchingElmJsonAndDocsFailed _ _ _ ->
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
                (project elmJson (modules "src") (modules "tests"))
                elmJson.deps
    in
    case Version.fromTuple ( 0, 19, 1 ) of
        Just version ->
            if Elm.Constraint.check version elmJson.elm then
                Review.Rule.reviewV2 [ NoUnused.Dependencies.rule ] Nothing projectWithDependencies
                    |> .errors
                    |> List.map
                        (\error ->
                            { message = Review.Rule.errorMessage error
                            , ruleName = Review.Rule.errorRuleName error
                            , filePath = Review.Rule.errorFilePath error
                            , details = Review.Rule.errorDetails error
                            , range = Review.Rule.errorRange error
                            }
                        )

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
