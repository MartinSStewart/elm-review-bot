module Evergreen.Migrate.V14 exposing (..)

import AssocList
import Dict
import Elm.Version exposing (Version)
import Evergreen.V12.Types as Old
import Evergreen.V14.Types as New
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


packageVersion : Old.PackageStatus -> Version
packageVersion packageStatus =
    case packageStatus of
        Old.Pending version _ ->
            version

        Old.Fetched { elmJson } ->
            elmJson.version

        Old.FetchedAndChecked { elmJson } ->
            elmJson.version

        Old.FetchingElmJsonAndDocsFailed version _ _ ->
            version


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { cachedPackages =
                Dict.map
                    (\_ versions ->
                        List.map
                            (\packageStatus ->
                                ( packageVersion packageStatus, migrationPackageStatus packageStatus )
                            )
                            versions
                            |> AssocList.fromList
                    )
                    old.cachedPackages
          , clients = old.clients
          , updateIndex = old.updateIndex
          }
        , Cmd.none
        )


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
