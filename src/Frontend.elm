module Frontend exposing (..)

import AssocList
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Element exposing (Element)
import Element.Background
import Element.Font
import Element.Input
import Elm.Package
import Elm.Version
import Html
import Http
import Lamdera
import List.Extra as List
import List.Nonempty
import PackageStatus exposing (ReviewResult(..), RunRuleResult(..))
import Review.Fix
import Types exposing (DisplayOrder(..), FrontendModel, FrontendMsg(..), LoginStatus(..), PackageStatusFrontend(..), ToBackend(..), ToFrontend(..))
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , state = Dict.empty
      , order = RequestOrder
      , loginStatus = NotLoggedIn ""
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        ToggleOrder ->
            ( { model
                | order =
                    case model.order of
                        RequestOrder ->
                            Alphabetical

                        Alphabetical ->
                            RequestOrder
              }
            , Cmd.none
            )

        PressedResetBackend ->
            ( { model | state = Dict.empty }, Lamdera.sendToBackend ResetBackend )

        PressedLogin ->
            ( model
            , case model.loginStatus of
                NotLoggedIn password ->
                    LoginRequest password |> Lamdera.sendToBackend

                LoggedIn ->
                    Cmd.none
            )

        TypedPassword password ->
            ( { model
                | loginStatus =
                    case model.loginStatus of
                        NotLoggedIn _ ->
                            NotLoggedIn password

                        LoggedIn ->
                            LoggedIn
              }
            , Cmd.none
            )

        PressedCreatePullRequest packageName ->
            ( model, Lamdera.sendToBackend (PullRequestRequest packageName) )

        PressedRerunPackage name version ->
            ( model, Lamdera.sendToBackend (RerunPackageRequest name version) )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        Updates dict ->
            ( { model
                | state =
                    Dict.merge
                        (\_ _ state -> state)
                        (\key a new state -> Dict.insert key (AssocList.union new a) state)
                        (\key new state -> Dict.insert key new state)
                        model.state
                        dict
                        model.state
                , loginStatus = LoggedIn
              }
            , Cmd.none
            )



--resetBackendButton =
--    Element.Input.button
--        buttonAttributes
--        { onPress = Just PressedResetBackend
--        , label = Element.text "Reset backend"
--        }
--
--
--resetRuleButton =
--    Element.Input.button
--        buttonAttributes
--        { onPress = Just PressedResetRules
--        , label = Element.text "Reset rules"
--        }


buttonAttributes =
    [ Element.Background.color <| Element.rgb 0.8 0.8 0.8
    , Element.padding 8
    ]


view : FrontendModel -> { title : String, body : List (Html.Html FrontendMsg) }
view model =
    { title = "elm-review-bot"
    , body =
        [ Element.layout
            [ Element.padding 16 ]
            (case model.loginStatus of
                NotLoggedIn password ->
                    Element.column
                        [ Element.spacing 8 ]
                        [ Element.Input.text [ Element.width <| Element.px 300 ]
                            { text = password
                            , placeholder = Nothing
                            , onChange = TypedPassword
                            , label = Element.Input.labelAbove [] (Element.text "Enter password")
                            }
                        , Element.Input.button buttonAttributes { onPress = Just PressedLogin, label = Element.text "Login" }
                        ]

                LoggedIn ->
                    Element.column
                        [ Element.width Element.fill, Element.spacing 8, Element.padding 8 ]
                        [ Element.row
                            [ Element.spacing 8 ]
                            [ case model.order of
                                RequestOrder ->
                                    Element.Input.button
                                        buttonAttributes
                                        { onPress = Just ToggleOrder
                                        , label = Element.text "Show alphabetical"
                                        }

                                Alphabetical ->
                                    Element.Input.button
                                        buttonAttributes
                                        { onPress = Just ToggleOrder
                                        , label = Element.text "Show requested order"
                                        }
                            , Element.text <| "Total packages: " ++ String.fromInt (Dict.size model.state)

                            --, resetBackendButton
                            --, resetRuleButton
                            ]
                        , packagesView model
                        ]
            )
        ]
    }


packagesView : FrontendModel -> Element FrontendMsg
packagesView model =
    Element.column
        [ Element.spacing 8 ]
        (Dict.toList model.state
            |> List.concatMap
                (\( packageName, versions ) ->
                    case Elm.Package.fromString packageName of
                        Just packageName_ ->
                            case
                                AssocList.toList versions
                                    |> List.maximumBy (Tuple.second >> Types.updateIndex)
                                    |> Maybe.map
                                        (\( version, packageStatus ) ->
                                            packageView (AssocList.size versions) packageName_ version packageStatus
                                        )
                            of
                                Just a ->
                                    [ a ]

                                Nothing ->
                                    []

                        Nothing ->
                            []
                )
            |> (case model.order of
                    RequestOrder ->
                        List.sortBy (\{ status } -> Types.updateIndex status)

                    Alphabetical ->
                        List.sortWith
                            (\a b ->
                                case compare (Elm.Package.toString a.name) (Elm.Package.toString b.name) of
                                    EQ ->
                                        Elm.Version.compare a.version b.version

                                    LT ->
                                        LT

                                    GT ->
                                        GT
                            )
               )
            |> List.map (\a -> a.view)
        )


packageView :
    Int
    -> Elm.Package.Name
    -> Elm.Version.Version
    -> PackageStatusFrontend
    -> { version : Elm.Version.Version, name : Elm.Package.Name, view : Element FrontendMsg, status : PackageStatusFrontend }
packageView count packageName version status =
    let
        packageVersionText =
            Elm.Version.toString version
    in
    { version = version
    , name = packageName
    , status = status
    , view =
        Element.column
            []
            (Element.paragraph []
                [ Element.newTabLink [ Element.Font.color <| Element.rgb 0 0 1 ]
                    { url = "https://github.com/" ++ Elm.Package.toString packageName ++ "/tree/" ++ packageVersionText
                    , label = Element.text (Elm.Package.toString packageName ++ " " ++ packageVersionText)
                    }
                , Element.text (" total: " ++ String.fromInt count)
                ]
                :: (case status of
                        Fetched_ _ ->
                            [ Element.text "Fetched" ]

                        FetchedAndChecked_ { result } ->
                            [ case result of
                                HttpError httpError ->
                                    Element.el [ errorColor ] (Element.text (httpErrorToString httpError))

                                CouldNotOpenDefaultBranchZip ->
                                    Element.el [ errorColor ] (Element.text "CouldNotOpenDefaultBranchZip")

                                PackageTagNotFound ->
                                    Element.el [ errorColor ] (Element.text "PackageTagNotFound")

                                RuleErrors runRuleResult ->
                                    showRuleResult packageName version runRuleResult
                            ]

                        FetchingElmJsonAndDocsFailed_ _ _ _ ->
                            [ Element.text "FetchingElmJsonAndDocsFailed" |> Element.el [ errorColor ] ]

                        FetchedCheckedAndPullRequestPending_ _ ->
                            [ Element.text "Pull request pending" ]

                        FetchedCheckedAndPullRequestSent_ _ ->
                            [ Element.text "Pull request sent" ]

                        FetchedCheckedAndPullRequestFailed_ { error } ->
                            [ createPullRequestButton False packageName
                            , Element.el
                                [ errorColor ]
                                (Element.text <|
                                    "Pull request error: "
                                        ++ Tuple.first error
                                        ++ " failed with "
                                        ++ httpErrorToString (Tuple.second error)
                                )
                            ]
                   )
            )
    }


createPullRequestButton : Bool -> Elm.Package.Name -> Element FrontendMsg
createPullRequestButton firstAttempt packageName =
    Element.Input.button
        buttonAttributes
        { onPress = PressedCreatePullRequest packageName |> Just
        , label =
            if firstAttempt then
                Element.text "Create pull request"

            else
                Element.text "Try again with pull request"
        }


rerunPackageButton : Elm.Package.Name -> Elm.Version.Version -> Element FrontendMsg
rerunPackageButton packageName version =
    Element.Input.button
        buttonAttributes
        { onPress = PressedRerunPackage packageName version |> Just
        , label = Element.text "Rerun package"
        }


showRuleResult : Elm.Package.Name -> Elm.Version.Version -> RunRuleResult -> Element FrontendMsg
showRuleResult packageName version ruleResult =
    case ruleResult of
        FoundErrors { errors, oldElmJson, newElmJson } ->
            createPullRequestButton True packageName
                :: List.map
                    (\{ ruleName, message } ->
                        Element.paragraph
                            []
                            [ Element.text <| ruleName ++ ": " ++ message ]
                    )
                    (List.Nonempty.toList errors)
                ++ [ Element.text "Before:"
                   , Element.text oldElmJson
                   , Element.text "After:"
                   , Element.text newElmJson
                   ]
                |> Element.column [ errorColor ]

        NoErrorsFounds ->
            Element.el
                [ Element.Font.color <| Element.rgb 0.1 0.7 0.1 ]
                (Element.text "Passed")

        ParsingError errors ->
            List.Nonempty.toList errors
                |> List.map (\error -> Element.paragraph [] [ Element.text error ])
                |> (::) (Element.text "Parsing errors: ")
                |> (::) (rerunPackageButton packageName version)
                |> Element.column [ errorColor ]

        IncorrectProject ->
            Element.paragraph [ errorColor ] [ Element.text "Incorrect project" ]

        NotEnoughIterations ->
            Element.paragraph [ errorColor ] [ Element.text "Not enough iterations" ]

        NotAnElm19xPackage ->
            Element.paragraph [ errorColor ] [ Element.text "Not an Elm 19.x package" ]

        DependenciesDontExist nonempty ->
            List.Nonempty.toList nonempty
                |> List.map Elm.Package.toString
                |> String.join ", "
                |> (++) "Missing dependencies: "
                |> Element.text
                |> List.singleton
                |> Element.paragraph [ errorColor ]

        FixFailed problem ->
            Element.paragraph [ errorColor ]
                [ case problem of
                    Review.Fix.Unchanged ->
                        Element.text "Tried to apply a fix that didn't change anything"

                    Review.Fix.SourceCodeIsNotValid message ->
                        "Source code not valid: " ++ message |> Element.text

                    Review.Fix.HasCollisionsInFixRanges ->
                        Element.text "HasCollisionsInFixRanges"
                ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadBody text ->
            text

        Http.BadUrl url ->
            "invalid url " ++ url

        Http.Timeout ->
            "timed out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus statusCode ->
            "nad status code " ++ String.fromInt statusCode


errorColor =
    Element.Font.color <| Element.rgb 0.8 0.1 0.1
