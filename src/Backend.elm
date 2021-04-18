module Backend exposing (..)

import AssocList
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Elm.Constraint
import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Version as Version exposing (Version)
import Env
import Github
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import List.Nonempty
import NoUnused.Dependencies
import PackageStatus exposing (PackageStatus(..), ReviewResult(..), RunRuleResult(..))
import Parser exposing ((|.), (|=), Parser)
import Process
import Review.Fix
import Review.Project
import Review.Project.Dependency
import Review.Rule
import Set exposing (Set)
import Task exposing (Task)
import Types exposing (BackendModel, BackendMsg(..), PackageEndpoint, ToBackend(..), ToFrontend(..))
import Zip exposing (Zip)
import Zip.Entry


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


getPackageDocs : Elm.Package.Name -> Version -> Task Http.Error (List Elm.Docs.Module)
getPackageDocs packageName version =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            "https://package.elm-lang.org/packages/"
                ++ Elm.Package.toString packageName
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
        , timeout = Just 30000
        }


getPackageElmJson : Elm.Package.Name -> Version -> Task Http.Error Elm.Project.Project
getPackageElmJson packageName version =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            "https://package.elm-lang.org/packages/"
                ++ Elm.Package.toString packageName
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
        , timeout = Just 30000
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
      --, Task.perform
      --    (\_ ->
      --        [ ( "elm/core", Version.fromString "1.0.1" )
      --        , ( "MartinSStewart/elm-serialize", Version.fromString "1.1.0" )
      --        , ( "MartinSStewart/elm-box-packing", Version.fromString "3.0.0" )
      --        , ( "elm/bytes", Version.fromString "1.0.8" )
      --        , ( "ianmackenzie/elm-units", Version.fromString "2.4.0" )
      --        , ( "justgook/elm-image", Version.fromString "4.0.0" )
      --        , ( "elm/random", Version.fromString "1.0.0" )
      --        , ( "elm-explorations/test", Version.fromString "1.2.2" )
      --        ]
      --            |> List.filterMap (\( a, b ) -> Maybe.map (Tuple.pair a) b)
      --            |> Ok
      --            |> GotNewPackagePreviews
      --    )
      --    (Task.succeed ())
      --, Task.perform
      --    (\_ ->
      --        [ ( "elm/core", Version.fromString "1.0.1" )
      --        , ( "avh4/elm-color", Version.fromString "1.0.0" )
      --        , ( "elm/browser", Version.fromString "1.0.0" )
      --        , ( "elm/html", Version.fromString "1.0.0" )
      --        , ( "elm/json", Version.fromString "1.0.0" )
      --        , ( "elm/time", Version.fromString "1.0.0" )
      --        , ( "rundis/elm-bootstrap", Version.fromString "5.2.0" )
      --        , ( "elm-explorations/test", Version.fromString "1.2.2" )
      --        ]
      --            |> List.filterMap (\( a, b ) -> Maybe.map (Tuple.pair a) b)
      --            |> Ok
      --            |> GotNewPackagePreviews
      --    )
      --    (Task.succeed ())
      --avh4/elm-color 1.0.0 <= v < 2.0.0
      --elm/browser 1.0.0 <= v < 2.0.0
      --elm/core 1.0.0 <= v < 2.0.0
      --elm/html 1.0.0 <= v < 2.0.0
      --elm/json 1.0.0 <= v < 2.0.0
      --elm/time 1.0.0 <= v < 2.0.0
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
                                                    let
                                                        value =
                                                            Maybe.withDefault AssocList.empty maybeValue
                                                    in
                                                    if AssocList.member version value then
                                                        maybeValue

                                                    else
                                                        AssocList.insert
                                                            version
                                                            (Pending version state.updateIndex)
                                                            value
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
                                { updateIndex = model.updateIndex
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
                                (Elm.Package.toString packageName)
                                (Maybe.map (AssocList.insert version packageStatus))
                                model.cachedPackages
                        , updateIndex = model.updateIndex + 1
                    }
            in
            ( model2
            , Cmd.batch (nextTodo model2 :: sendChange model.clients packageName packageStatus)
            )

        RanNoUnused { packageName, elmJson, docs } result ->
            let
                packageStatus =
                    FetchedAndChecked
                        { updateIndex = model.updateIndex
                        , docs = docs
                        , elmJson = elmJson
                        , result = result
                        }

                model2 =
                    { model
                        | cachedPackages =
                            Dict.update
                                (Elm.Package.toString packageName)
                                (Maybe.map (AssocList.insert elmJson.version packageStatus))
                                model.cachedPackages
                        , updateIndex = model.updateIndex + 1
                    }
            in
            ( model2
            , Cmd.batch (nextTodo model2 :: sendChange model.clients packageName packageStatus)
            )

        ClientConnected sessionId clientId ->
            ( model
            , if Set.member sessionId model.clients then
                sendUpdates clientId model

              else
                Cmd.none
            )

        ClientDisconnected _ clientId ->
            ( { model | clients = Set.remove clientId model.clients }, Cmd.none )

        CreatePullRequestResult packageName version result ->
            let
                model2 =
                    { model
                        | cachedPackages =
                            Dict.update
                                (Elm.Package.toString packageName)
                                (\maybePackages ->
                                    case maybePackages of
                                        Just packages ->
                                            AssocList.update version
                                                (Maybe.map
                                                    (\packageStatus ->
                                                        case result of
                                                            Ok ok ->
                                                                PackageStatus.pullRequestSent ok packageStatus

                                                            Err err ->
                                                                PackageStatus.pullRequestFailed err packageStatus
                                                    )
                                                )
                                                packages
                                                |> Just

                                        Nothing ->
                                            Nothing
                                )
                                model.cachedPackages
                    }
            in
            ( model2
            , case Dict.get (Elm.Package.toString packageName) model2.cachedPackages of
                Just packages ->
                    case AssocList.get version packages of
                        Just packageStatus ->
                            Cmd.batch (sendChange model.clients packageName packageStatus)

                        Nothing ->
                            Cmd.none

                Nothing ->
                    Cmd.none
            )


sendChange : Set ClientId -> Elm.Package.Name -> PackageStatus -> List (Cmd backendMsg)
sendChange clients packageName packageStatus =
    List.map
        (\client ->
            Lamdera.sendToFrontend
                client
                (Updates
                    (Dict.singleton
                        (Elm.Package.toString packageName)
                        (List.filterMap Types.statusToStatusFrontend [ packageStatus ])
                    )
                )
        )
        (Set.toList clients)


sendUpdates : ClientId -> BackendModel -> Cmd backendMsg
sendUpdates clientId model =
    Dict.map (\_ value -> AssocList.values value |> List.filterMap Types.statusToStatusFrontend) model.cachedPackages
        |> Updates
        |> Lamdera.sendToFrontend clientId


nextTodo : BackendModel -> Cmd BackendMsg
nextTodo model =
    let
        maybeNextTodo : Maybe ( Elm.Package.Name, PackageStatus )
        maybeNextTodo =
            Dict.foldl
                (\packageName versions currentTodo ->
                    case Elm.Package.fromString packageName of
                        Just packageName_ ->
                            List.foldl
                                (\packageStatus currentTodo_ ->
                                    case packageStatus of
                                        Pending _ _ ->
                                            Just ( packageName_, packageStatus )

                                        Fetched { elmJson } ->
                                            if String.startsWith "elm/" packageName then
                                                currentTodo_

                                            else
                                                case currentTodo_ of
                                                    Just ( _, Pending _ _ ) ->
                                                        currentTodo_

                                                    _ ->
                                                        if
                                                            List.count
                                                                (Types.packageVersion
                                                                    >> Version.compare elmJson.version
                                                                    >> (/=) GT
                                                                )
                                                                (AssocList.values versions)
                                                                == 1
                                                        then
                                                            Just ( packageName_, packageStatus )

                                                        else
                                                            currentTodo_

                                        FetchedAndChecked _ ->
                                            currentTodo_

                                        FetchingElmJsonAndDocsFailed _ _ _ ->
                                            currentTodo_

                                        FetchedCheckedAndPullRequestPending _ ->
                                            currentTodo_

                                        FetchedCheckedAndPullRequestSent _ _ ->
                                            currentTodo_

                                        FetchedCheckedAndPullRequestFailed _ _ ->
                                            currentTodo_
                                )
                                currentTodo
                                (AssocList.values versions)

                        Nothing ->
                            currentTodo
                )
                Nothing
                model.cachedPackages
    in
    case maybeNextTodo of
        Just ( packageName, Pending version _ ) ->
            Process.sleep 200
                |> Task.andThen (\_ -> getPackageElmJson packageName version)
                |> Task.andThen
                    (\elmJson ->
                        getPackageDocs packageName version
                            |> Task.map (List.map PackageStatus.removeComments >> Tuple.pair elmJson)
                    )
                |> Task.attempt (FetchedElmJsonAndDocs { packageName = packageName, version = version })

        Just ( packageName, Fetched { elmJson, docs } ) ->
            let
                ( owner, repo ) =
                    ownerAndRepo packageName
            in
            Process.sleep 200
                |> Task.andThen (\_ -> reportErrors owner repo elmJson model)
                |> Task.perform (RanNoUnused { packageName = packageName, elmJson = elmJson, docs = docs })

        Just ( _, FetchedAndChecked _ ) ->
            Cmd.none

        Just ( _, FetchingElmJsonAndDocsFailed _ _ _ ) ->
            Cmd.none

        Just ( _, FetchedCheckedAndPullRequestPending _ ) ->
            Cmd.none

        Just ( _, FetchedCheckedAndPullRequestSent _ _ ) ->
            Cmd.none

        Just ( _, FetchedCheckedAndPullRequestFailed _ _ ) ->
            Cmd.none

        Nothing ->
            Cmd.none


ownerAndRepo : Elm.Package.Name -> ( Github.Owner, String )
ownerAndRepo packageName =
    case Elm.Package.toString packageName |> String.split "/" of
        [ originalOwner_, originalRepo_ ] ->
            ( Github.owner originalOwner_, originalRepo_ )

        _ ->
            -- This should never happen
            ( Github.owner "", "" )


createPullRequest : Int -> Bool -> String -> Github.Owner -> String -> String -> Task Http.Error { url : String }
createPullRequest changeCount onlyTestDependencies elmJsonContent originalOwner originalRepo branchName =
    Github.createFork
        { authToken = Env.githubAuth, owner = originalOwner, repo = originalRepo }
        |> Task.andThen
            (\fork ->
                Github.getBranch
                    { authToken = Env.githubAuth
                    , repo = fork.repo
                    , owner = fork.owner
                    , branchName = branchName
                    }
                    |> Task.andThen
                        (\commitSha ->
                            Github.getCommit
                                { authToken = Env.githubAuth
                                , repo = fork.repo
                                , owner = fork.owner
                                , sha = commitSha
                                }
                                |> Task.andThen
                                    (\treeSha ->
                                        Github.createTree
                                            { authToken = Env.githubAuth
                                            , owner = fork.owner
                                            , repo = fork.repo
                                            , treeNodes =
                                                [ { path = "elm.json"
                                                  , content = elmJsonContent
                                                  }
                                                ]
                                            , baseTree = Just treeSha
                                            }
                                            |> Task.andThen
                                                (\tree ->
                                                    Github.createCommit
                                                        { authToken = Env.githubAuth
                                                        , repo = fork.repo
                                                        , owner = fork.owner
                                                        , message = "Remove unused dependencies"
                                                        , tree = tree.treeSha
                                                        , parents = [ commitSha ]
                                                        }
                                                )
                                    )
                        )
                    |> Task.andThen
                        (\commitSha ->
                            Github.updateBranch
                                { authToken = Env.githubAuth
                                , owner = fork.owner
                                , repo = fork.repo
                                , branchName = branchName
                                , sha = commitSha
                                , force = False
                                }
                        )
                    |> Task.andThen
                        (\_ ->
                            Github.createPullRequest
                                { authToken = Env.githubAuth
                                , sourceBranchOwner = fork.owner
                                , destinationOwner = originalOwner
                                , destinationRepo = fork.repo
                                , destinationBranch = branchName
                                , sourceBranch = branchName
                                , title = "Remove unused dependencies"
                                , description = pullRequestMessage changeCount onlyTestDependencies
                                }
                        )
            )


pullRequestMessage : Int -> Bool -> String
pullRequestMessage changeCount onlyTestDependencies =
    "Hello :wave:!\n\n"
        ++ (if changeCount == 1 then
                "I noticed an unused dependency in your package. Here is a pull request to remove it."

            else
                "I noticed there were unused dependencies in your package. Here is a pull request to remove them."
           )
        ++ (if onlyTestDependencies then
                "\n"

            else
                " After this gets merged, I recommend publishing a new release, unless you are working on something else in the meantime.\n"
           )
        ++ """
I found this issue using [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) and the [NoUnused.Dependencies](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Dependencies) rule from [`jfmengels/elm-review-unused`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/). You can re-create my findings by running this command:

```bash
npx elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Dependencies
```

If you like these findings and want to find more dead code in your code, you can add `elm-review` to your project like this:

```bash
npx elm-review init --template jfmengels/elm-review-unused/example
# then to run it:
npx elm-review # reports problems
npx elm-review --fix # fixes the issue.
```
More information on how to get started in the [`elm-review` documentation](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/), and you can read more about [how dead code removal](https://jfmengels.net/safe-dead-code-removal/) is done using this tool.

This pull request was made automatically (by @MartinSStewart). You can tell me to stop making pull requests like this by writing "please stop".

Have a nice day!"""


reportErrors : Github.Owner -> String -> Elm.Project.PackageInfo -> BackendModel -> Task Never ReviewResult
reportErrors owner repo elmJson model =
    Github.getRepository { authToken = Env.githubAuth, repo = repo, owner = owner }
        |> Task.andThen
            (\{ defaultBranch } ->
                Task.map3 (\a b c -> ( a, b, c ))
                    (Github.getBranch
                        { authToken = Env.githubAuth
                        , repo = repo
                        , owner = owner
                        , branchName = defaultBranch
                        }
                    )
                    (Github.getTag
                        { authToken = Env.githubAuth
                        , repo = repo
                        , owner = owner
                        , tagName = Version.toString elmJson.version
                        }
                    )
                    (getPackageEndpoint (Elm.Package.toString elmJson.name) elmJson.version
                        |> Task.andThen getPackageZip
                    )
                    |> Task.andThen
                        (\( branchSha, tagSha, bytes ) ->
                            case Zip.fromBytes bytes of
                                Just zip ->
                                    let
                                        result : RunRuleResult
                                        result =
                                            checkPackage elmJson model.cachedPackages zip
                                    in
                                    if branchSha == tagSha then
                                        RuleErrors result |> Task.succeed
                                        --createPullRequest
                                        --    (List.length errors)
                                        --    elmJsonText
                                        --    owner
                                        --    repo
                                        --    defaultBranch
                                        --    |> Task.map (\{ url } -> RuleErrorsAndPullRequest { errors = errors, pullRequestUrl = url })

                                    else
                                        RuleErrors result |> Task.succeed

                                Nothing ->
                                    Task.succeed CouldNotOpenDefaultBranchZip
                        )
            )
        |> Task.onError (HttpError >> Task.succeed)


project :
    Elm.Project.PackageInfo
    -> List { path : String, source : String }
    -> List { path : String, source : String }
    -> Review.Project.Project
project elmJson srcModules testModules =
    let
        isWhitespace : Char -> Bool
        isWhitespace c =
            c == ' ' || c == '\t' || c == '\n'

        importParser : Parser String
        importParser =
            Parser.succeed identity
                |. Parser.chompWhile isWhitespace
                |= (Parser.chompWhile (isWhitespace >> not) |> Parser.getChompedString)

        importText =
            "\nimport"

        srcModules_ : Dict String { path : String, source : String, imports : List String }
        srcModules_ =
            srcModules
                |> List.map
                    (\{ path, source } ->
                        ( String.split "/" path |> List.drop 1 |> String.join "." |> String.dropRight (String.length ".elm")
                        , { path = path
                          , source = source
                          , imports =
                                String.filter ((/=) '\u{000D}') source
                                    |> String.indexes importText
                                    |> List.filterMap
                                        (\index ->
                                            String.slice (index + String.length importText) (index + 200) source
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
        |> Review.Project.addElmJson
            { path = "src/elm.json"
            , raw = Elm.Project.Package elmJson |> Elm.Project.encode |> Json.Encode.encode 0
            , project = Elm.Project.Package elmJson
            }


checkPackage : Elm.Project.PackageInfo -> Dict String (AssocList.Dict Version PackageStatus) -> Zip -> RunRuleResult
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
                            ( String.split "/" (Zip.Entry.path zipEntry) |> List.drop 1 |> List.Nonempty.fromList
                            , Zip.Entry.toString zipEntry
                            )
                        of
                            ( Just nonemptyPath, Ok source ) ->
                                if
                                    String.endsWith ".elm" (List.Nonempty.last nonemptyPath)
                                        && (List.Nonempty.get 0 nonemptyPath == folderName)
                                then
                                    Just
                                        { path = String.join "/" (List.Nonempty.toList nonemptyPath)
                                        , source = source
                                        }

                                else
                                    Nothing

                            _ ->
                                Nothing
                )
                (Zip.ls zip)

        ( projectWithDependencies, missingPackages ) =
            List.foldl
                (\( packageName, constraint ) ( state, missingPackages_ ) ->
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

                                                FetchingElmJsonAndDocsFailed _ _ _ ->
                                                    Nothing

                                                FetchedCheckedAndPullRequestPending data ->
                                                    Just
                                                        ( Types.packageVersion package
                                                        , ( data.elmJson, data.docs )
                                                        )

                                                FetchedCheckedAndPullRequestSent data _ ->
                                                    Just
                                                        ( Types.packageVersion package
                                                        , ( data.elmJson, data.docs )
                                                        )

                                                FetchedCheckedAndPullRequestFailed data _ ->
                                                    Just
                                                        ( Types.packageVersion package
                                                        , ( data.elmJson, data.docs )
                                                        )

                                        else
                                            Nothing
                                    )
                                    (AssocList.values packages)
                                    |> List.maximumWith (\( a, _ ) ( b, _ ) -> Version.compare a b)
                            of
                                Just ( _, ( elmJson_, docs_ ) ) ->
                                    ( addDependency elmJson_ docs_ state, missingPackages_ )

                                Nothing ->
                                    ( state, missingPackages_ )

                        Nothing ->
                            ( state, packageName :: missingPackages_ )
                )
                ( project elmJson (modules "src") (modules "tests"), [] )
                (elmJson.deps ++ elmJson.testDeps)

        addDependency : Elm.Project.PackageInfo -> List Elm.Docs.Module -> Review.Project.Project -> Review.Project.Project
        addDependency elmJson_ docs =
            Review.Project.addDependency
                (Review.Project.Dependency.create (Elm.Package.toString elmJson_.name) (Elm.Project.Package elmJson_) docs)
    in
    case List.Nonempty.fromList missingPackages of
        Just missingPackages_ ->
            DependenciesDontExist missingPackages_

        Nothing ->
            case Version.fromTuple ( 0, 19, 1 ) of
                Just version ->
                    if Elm.Constraint.check version elmJson.elm then
                        runRule
                            10
                            (Elm.Project.encode (Elm.Project.Package elmJson) |> Json.Encode.encode 4)
                            NoUnused.Dependencies.rule
                            Nothing
                            []
                            projectWithDependencies

                    else
                        NotAnElm19xPackage

                Nothing ->
                    -- Should never happen
                    NotAnElm19xPackage


runRule :
    Int
    -> String
    -> Review.Rule.Rule
    -> Maybe Review.Rule.ProjectData
    -> List Review.Rule.ReviewError
    -> Review.Project.Project
    -> RunRuleResult
runRule stepsLeft originalElmJson rule projectData errors projectWithDependencies =
    let
        result : { errors : List Review.Rule.ReviewError, rules : List Review.Rule.Rule, projectData : Maybe Review.Rule.ProjectData }
        result =
            Review.Rule.reviewV2 [ rule ] projectData projectWithDependencies

        elmJsonFixes : List ( Review.Rule.ReviewError, List Review.Fix.Fix )
        elmJsonFixes =
            List.filterMap
                (\error ->
                    if Review.Rule.errorFilePath error == "src/elm.json" then
                        case Review.Rule.errorFixes error of
                            Just fixes ->
                                Just ( error, fixes )

                            Nothing ->
                                Nothing

                    else
                        Nothing
                )
                result.errors
    in
    if stepsLeft <= 0 then
        NotEnoughIterations

    else if List.any (\error -> Review.Rule.errorRuleName error == "ParsingError") result.errors then
        ParsingError

    else if List.any (\error -> Review.Rule.errorRuleName error == "Incorrect project") result.errors then
        IncorrectProject

    else
        case ( elmJsonFixes, Review.Project.elmJson projectWithDependencies ) of
            ( ( error, fixes ) :: _, Just elmJson ) ->
                case Review.Fix.fix (Review.Rule.errorTarget error) fixes elmJson.raw of
                    Review.Fix.Successful newRaw ->
                        case Json.Decode.decodeString Elm.Project.decoder newRaw of
                            Ok elmJsonParsed ->
                                Review.Project.addElmJson
                                    { elmJson | raw = newRaw, project = elmJsonParsed }
                                    projectWithDependencies
                                    |> runRule (stepsLeft - 1) originalElmJson rule projectData (error :: errors)

                            Err _ ->
                                FixFailed (Review.Fix.SourceCodeIsNotValid "elm.json is now an application")

                    Review.Fix.Errored fixError ->
                        FixFailed fixError

            ( [], Just elmJson ) ->
                case List.Nonempty.fromList errors of
                    Just nonemptyErrors ->
                        FoundErrors
                            { errors =
                                List.Nonempty.map
                                    (\error ->
                                        { message = Review.Rule.errorMessage error
                                        , ruleName = Review.Rule.errorRuleName error
                                        , filePath = Review.Rule.errorFilePath error
                                        , details = Review.Rule.errorDetails error
                                        , range = Review.Rule.errorRange error
                                        }
                                    )
                                    nonemptyErrors
                            , oldElmJson = originalElmJson
                            , newElmJson = elmJson.raw
                            }

                    Nothing ->
                        NoErrorsFounds

            _ ->
                IncorrectProject


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        authenticate : ( BackendModel, Cmd BackendMsg ) -> ( BackendModel, Cmd BackendMsg )
        authenticate tuple =
            if Set.member sessionId model.clients then
                tuple

            else
                ( model, Cmd.none )
    in
    case msg of
        ResetBackend ->
            init |> Tuple.mapFirst (\m -> { m | clients = model.clients })

        LoginRequest password ->
            if password == Env.adminPassword then
                ( { model | clients = Set.insert sessionId model.clients }
                , if Set.member password model.clients then
                    Cmd.none

                  else
                    sendUpdates clientId model
                )

            else
                ( model, Cmd.none )

        ResetRules ->
            authenticate
                ( { model
                    | cachedPackages =
                        Dict.map
                            (\_ versions ->
                                AssocList.map
                                    (\_ packageStatus ->
                                        case packageStatus of
                                            Fetched data ->
                                                Fetched data

                                            Pending version updateIndex ->
                                                Pending version updateIndex

                                            FetchedAndChecked { updateIndex, docs, elmJson } ->
                                                Fetched { updateIndex = updateIndex, docs = docs, elmJson = elmJson }

                                            FetchingElmJsonAndDocsFailed version updateIndex _ ->
                                                Pending version updateIndex

                                            FetchedCheckedAndPullRequestPending fetchedAndChecked_ ->
                                                FetchedCheckedAndPullRequestPending fetchedAndChecked_

                                            FetchedCheckedAndPullRequestSent fetchedAndChecked_ record ->
                                                FetchedCheckedAndPullRequestSent fetchedAndChecked_ record

                                            FetchedCheckedAndPullRequestFailed fetchedAndChecked_ error ->
                                                FetchedCheckedAndPullRequestFailed fetchedAndChecked_ error
                                    )
                                    versions
                            )
                            model.cachedPackages
                  }
                , getAllPackages packageCountOffset
                )

        PullRequestRequest packageName ->
            authenticate
                (case ( Dict.get (Elm.Package.toString packageName) model.cachedPackages, ownerAndRepo packageName ) of
                    ( Just packageVersions, ( owner, repo ) ) ->
                        case AssocList.toList packageVersions |> List.maximumBy (Tuple.first >> Version.toTuple) of
                            Just ( version, FetchedAndChecked latestPackage ) ->
                                case PackageStatus.getRunRuleResult latestPackage.result of
                                    Just (FoundErrors ({ errors, newElmJson } as foundErrors)) ->
                                        ( { model
                                            | cachedPackages =
                                                Dict.update
                                                    (Elm.Package.toString packageName)
                                                    (\maybePackages ->
                                                        case maybePackages of
                                                            Just packages ->
                                                                AssocList.update version
                                                                    (Maybe.map (PackageStatus.pullRequestPending foundErrors))
                                                                    packages
                                                                    |> Just

                                                            Nothing ->
                                                                Nothing
                                                    )
                                                    model.cachedPackages
                                          }
                                        , Cmd.batch
                                            [ Github.getRepository
                                                { authToken = Env.githubAuth, repo = repo, owner = owner }
                                                |> Task.andThen
                                                    (\{ defaultBranch } ->
                                                        createPullRequest
                                                            (List.Nonempty.length errors)
                                                            (List.Nonempty.any (.message >> String.startsWith "Unused test dependency ") errors)
                                                            newElmJson
                                                            owner
                                                            repo
                                                            defaultBranch
                                                    )
                                                |> Task.attempt (CreatePullRequestResult packageName version)
                                            ]
                                        )

                                    _ ->
                                        ( model, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )
                )
