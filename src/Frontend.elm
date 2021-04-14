module Frontend exposing (..)

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
import Review.Fix
import Types exposing (..)
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

        PressedCreateFork ->
            ( model
            , Cmd.none
              --, Backend.createPullRequest 1 "test" "MartinSStewart" "elm-serialize" "master"
              --    |> Task.attempt CreateForkResult
            )

        PressedResetBackend ->
            ( { model | state = Dict.empty }, Lamdera.sendToBackend ResetBackend )

        PressedResetRules ->
            ( { model
                | state =
                    Dict.map
                        (\_ versions ->
                            List.filterMap
                                (\packageStatus ->
                                    case packageStatus of
                                        Fetched_ data ->
                                            Fetched_ data |> Just

                                        FetchedAndChecked_ { version, updateIndex } ->
                                            Fetched_ { version = version, updateIndex = updateIndex }
                                                |> Just

                                        FetchingElmJsonAndDocsFailed_ version updateIndex _ ->
                                            Nothing
                                )
                                versions
                        )
                        model.state
              }
            , Lamdera.sendToBackend ResetRules
            )

        CreateForkResult result ->
            ( model, Cmd.none )

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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        Updates dict ->
            ( { model
                | state =
                    Dict.merge
                        (\_ _ state -> state)
                        (\key a new state -> Dict.insert key (a ++ new) state)
                        (\key new state -> Dict.insert key new state)
                        model.state
                        dict
                        model.state
                , loginStatus = LoggedIn
              }
            , Cmd.none
            )


createPullRequestButton =
    Element.Input.button
        buttonAttributes
        { onPress = Just PressedCreateFork
        , label = Element.text "Create pull request"
        }


resetBackendButton =
    Element.Input.button
        buttonAttributes
        { onPress = Just PressedResetBackend
        , label = Element.text "Reset backend"
        }


resetRuleButton =
    Element.Input.button
        buttonAttributes
        { onPress = Just PressedResetRules
        , label = Element.text "Reset rules"
        }


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
                            , createPullRequestButton
                            , resetBackendButton
                            , resetRuleButton
                            ]
                        , packagesView model
                        ]
            )
        ]
    }


packagesView : FrontendModel -> Element msg
packagesView model =
    Element.column
        [ Element.spacing 8 ]
        (Dict.toList model.state
            |> List.concatMap
                (\( packageName, versions ) ->
                    case List.maximumBy Types.updateIndex versions |> Maybe.map (packageView packageName) of
                        Just a ->
                            [ a ]

                        Nothing ->
                            []
                )
            |> (case model.order of
                    RequestOrder ->
                        List.sortBy (\( a, _, _ ) -> Types.updateIndex a)

                    Alphabetical ->
                        List.sortWith
                            (\( b0, a0, _ ) ( b1, a1, _ ) ->
                                case compare a0 a1 of
                                    EQ ->
                                        Elm.Version.compare (Types.packageVersion_ b0) (Types.packageVersion_ b1)

                                    LT ->
                                        LT

                                    GT ->
                                        GT
                            )
               )
            |> List.map (\( _, _, a ) -> a)
        )


packageView : String -> PackageStatusFrontend -> ( PackageStatusFrontend, String, Element msg )
packageView packageName status =
    ( status
    , packageName
    , Element.column
        []
        [ Element.text (packageName ++ " " ++ Elm.Version.toString (Types.packageVersion_ status))
        , case status of
            Fetched_ _ ->
                Element.text "Fetched"

            FetchedAndChecked_ { result } ->
                case result of
                    RuleErrorsFromDefaultBranch ruleResult ->
                        Element.column []
                            [ Element.el [ errorColor ] <| Element.text "RuleErrorsFromDefaultBranch"
                            , showRuleResult ruleResult
                            ]

                    RuleErrorsFromTag ruleResult ->
                        Element.column []
                            [ Element.el [ errorColor ] <| Element.text "RuleErrorsFromTag"
                            , showRuleResult ruleResult
                            ]

                    RuleErrorsAndDefaultBranchAndTagMatch ruleResult ->
                        showRuleResult ruleResult

                    HttpError httpError ->
                        Element.el [ errorColor ] (Element.text (httpErrorToString httpError))

                    CouldNotOpenDefaultBranchZip ->
                        Element.el [ errorColor ] (Element.text "CouldNotOpenDefaultBranchZip")

                    CouldNotOpenTagZip ->
                        Element.el [ errorColor ] (Element.text "CouldNotOpenTagZip")

                    PackageTagNotFound ->
                        Element.el [ errorColor ] (Element.text "PackageTagNotFound")

                    InvalidPackageName ->
                        Element.el [ errorColor ] (Element.text "InvalidPackageName")

            FetchingElmJsonAndDocsFailed_ version int error ->
                Element.text "FetchingElmJsonAndDocsFailed" |> Element.el [ errorColor ]
        ]
    )


showRuleResult : RunRuleResult PullRequestStatus -> Element msg
showRuleResult ruleResult =
    case ruleResult of
        RunRuleSuccessful { errors, oldElmJson, newElmJson } pullRequestStatus ->
            if List.isEmpty errors then
                Element.el
                    [ Element.Font.color <| Element.rgb 0.1 0.7 0.1 ]
                    (Element.text "Passed")

            else
                List.map
                    (\{ ruleName, message } ->
                        Element.paragraph
                            []
                            [ Element.text <| ruleName ++ ": " ++ message ]
                    )
                    errors
                    ++ [ Element.text "Before:"
                       , Element.text oldElmJson
                       , Element.text "After:"
                       , Element.text newElmJson
                       ]
                    |> Element.column [ errorColor ]

        ParsingError ->
            Element.paragraph [ errorColor ] [ Element.text "Parsing error" ]

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


httpErrorToString error =
    case error of
        Http.BadBody text ->
            text

        Http.BadUrl url ->
            "Invalid url " ++ url

        Http.Timeout ->
            "Timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus statusCode ->
            "Bad status code  " ++ String.fromInt statusCode


errorColor =
    Element.Font.color <| Element.rgb 0.8 0.1 0.1
