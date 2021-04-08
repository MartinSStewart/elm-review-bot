module Evergreen.Migrate.V5 exposing (..)

import Dict
import Elm.Version as Version exposing (Version)
import Evergreen.V4.Types as Old
import Evergreen.V5.Types as New
import Http
import Json.Decode exposing (Decoder)
import Lamdera.Migrations exposing (..)
import Set


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


migrationPackageStatus : Old.PackageStatus -> New.PackageStatus
migrationPackageStatus packageStatus =
    case packageStatus of
        Old.Pending version updateIndex ->
            New.Pending version updateIndex

        Old.Fetched record ->
            New.Fetched
                { updateIndex = record.updateIndex
                , docs = record.docs
                , elmJson = record.elmJson
                }

        Old.FetchedAndChecked record ->
            New.Fetched
                { updateIndex = record.updateIndex
                , docs = record.docs
                , elmJson = record.elmJson
                }

        Old.FetchingElmJsonAndDocsFailed version updateIndex _ ->
            New.Pending version updateIndex


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { cachedPackages =
                Dict.map
                    (\_ versions ->
                        List.map migrationPackageStatus versions
                    )
                    old.cachedPackages
          , clients = Set.empty
          , updateIndex = old.updateIndex
          }
        , getAllPackages packageCountOffset
        )


packageCountOffset =
    6557


getAllPackages : Int -> Cmd New.BackendMsg
getAllPackages cachedCount =
    Http.get
        { url = "https://package.elm-lang.org/all-packages/since/" ++ String.fromInt cachedCount
        , expect = Http.expectJson New.GotNewPackagePreviews decodeAllPackages
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


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
