module Evergreen.Migrate.V33 exposing (..)

import AssocList
import Dict
import Evergreen.V32.PackageStatus as OldPackageStatus
import Evergreen.V32.Types as Old
import Evergreen.V33.PackageStatus as NewPackageStatus
import Evergreen.V33.Types as New
import Lamdera.Migrations exposing (..)
import List.Nonempty


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


migrateRunRuleResult : OldPackageStatus.RunRuleResult -> NewPackageStatus.RunRuleResult
migrateRunRuleResult ruleResult =
    case ruleResult of
        OldPackageStatus.ParsingError errors ->
            NewPackageStatus.ParsingError errors

        OldPackageStatus.IncorrectProject ->
            NewPackageStatus.IncorrectProject

        OldPackageStatus.FixFailed problem ->
            NewPackageStatus.FixFailed problem

        OldPackageStatus.NotEnoughIterations ->
            NewPackageStatus.NotEnoughIterations

        OldPackageStatus.NotAnElm19xPackage ->
            NewPackageStatus.NotAnElm19xPackage

        OldPackageStatus.DependenciesDontExist nonempty ->
            NewPackageStatus.DependenciesDontExist nonempty

        OldPackageStatus.FoundErrors foundErrors_ ->
            NewPackageStatus.FoundErrors foundErrors_

        OldPackageStatus.NoErrorsFounds ->
            NewPackageStatus.NoErrorsFounds


migrateReviewResult : OldPackageStatus.ReviewResult -> NewPackageStatus.ReviewResult
migrateReviewResult reviewResult =
    case reviewResult of
        OldPackageStatus.CouldNotOpenDefaultBranchZip ->
            NewPackageStatus.CouldNotOpenDefaultBranchZip

        OldPackageStatus.PackageTagNotFound ->
            NewPackageStatus.PackageTagNotFound

        OldPackageStatus.HttpError error ->
            NewPackageStatus.HttpError error

        OldPackageStatus.RuleErrors runRuleResult ->
            NewPackageStatus.RuleErrors (migrateRunRuleResult runRuleResult)


migrationPackageStatus : OldPackageStatus.PackageStatus -> NewPackageStatus.PackageStatus
migrationPackageStatus packageStatus =
    case packageStatus of
        OldPackageStatus.Pending version updateIndex ->
            NewPackageStatus.Pending version updateIndex

        OldPackageStatus.Fetched record ->
            NewPackageStatus.Fetched record

        OldPackageStatus.FetchedAndChecked record ->
            NewPackageStatus.FetchedAndChecked
                { updateIndex = record.updateIndex
                , docs = record.docs
                , elmJson = record.elmJson
                , result = migrateReviewResult record.result
                }

        OldPackageStatus.FetchingElmJsonAndDocsFailed version updateIndex _ ->
            NewPackageStatus.Pending version updateIndex

        OldPackageStatus.FetchedCheckedAndPullRequestPending fetchedAndChecked_ ->
            NewPackageStatus.FetchedCheckedAndPullRequestPending fetchedAndChecked_

        OldPackageStatus.FetchedCheckedAndPullRequestSent fetchedAndChecked_ record ->
            NewPackageStatus.FetchedCheckedAndPullRequestSent fetchedAndChecked_ record

        OldPackageStatus.FetchedCheckedAndPullRequestFailed fetchedAndChecked_ error ->
            NewPackageStatus.FetchedCheckedAndPullRequestFailed fetchedAndChecked_ ( "", error )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { cachedPackages =
                Dict.map
                    (\_ versions -> AssocList.map (\_ packageStatus -> migrationPackageStatus packageStatus) versions)
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
