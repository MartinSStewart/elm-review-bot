module Evergreen.Migrate.V19 exposing (..)

import AssocList
import Dict
import Elm.Docs
import Evergreen.V16.Types as Old
import Evergreen.V19.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


migrateRunRuleResult : Old.RunRuleResult Old.PullRequestStatus -> New.RunRuleResult New.PullRequestStatus
migrateRunRuleResult ruleResult =
    case ruleResult of
        Old.ParsingError ->
            New.ParsingError

        Old.IncorrectProject ->
            New.IncorrectProject

        Old.FixFailed problem ->
            New.FixFailed problem

        Old.NotEnoughIterations ->
            New.NotEnoughIterations

        Old.RunRuleSuccessful record _ ->
            New.RunRuleSuccessful record New.PullRequestNotSent

        Old.NotAnElm19xPackage ->
            New.NotAnElm19xPackage

        Old.DependenciesDontExist nonempty ->
            New.DependenciesDontExist nonempty


migrateReviewResult : Old.ReviewResult -> New.ReviewResult
migrateReviewResult reviewResult =
    case reviewResult of
        Old.CouldNotOpenDefaultBranchZip ->
            New.CouldNotOpenDefaultBranchZip

        Old.PackageTagNotFound ->
            New.PackageTagNotFound

        Old.HttpError error ->
            New.HttpError error

        Old.InvalidPackageName ->
            New.InvalidPackageName

        Old.RuleErrorsFromDefaultBranch runRuleResult ->
            New.RuleErrorsFromTag (migrateRunRuleResult runRuleResult)

        Old.RuleErrorsFromTag runRuleResult ->
            New.RuleErrorsFromTag (migrateRunRuleResult runRuleResult)

        Old.RuleErrorsAndDefaultBranchAndTagMatch runRuleResult ->
            New.RuleErrorsAndDefaultBranchAndTagMatch (migrateRunRuleResult runRuleResult)


removeComments : Elm.Docs.Module -> Elm.Docs.Module
removeComments docs =
    { name = docs.name
    , comment = ""
    , unions = List.map (\union -> { union | comment = "" }) docs.unions
    , aliases = List.map (\alias -> { alias | comment = "" }) docs.aliases
    , values = List.map (\value -> { value | comment = "" }) docs.values
    , binops = List.map (\binop -> { binop | comment = "" }) docs.binops
    }


migrationPackageStatus : Old.PackageStatus -> New.PackageStatus
migrationPackageStatus packageStatus =
    case packageStatus of
        Old.Pending version updateIndex ->
            New.Pending version updateIndex

        Old.Fetched record ->
            New.Fetched
                { updateIndex = record.updateIndex
                , docs = List.map removeComments record.docs
                , elmJson = record.elmJson
                }

        Old.FetchedAndChecked record ->
            New.FetchedAndChecked
                { updateIndex = record.updateIndex
                , docs = List.map removeComments record.docs
                , elmJson = record.elmJson
                , result = migrateReviewResult record.result
                }

        Old.FetchingElmJsonAndDocsFailed version updateIndex _ ->
            New.Pending version updateIndex


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
