module Frontend exposing (..)

import Backend
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Element exposing (Element)
import Element.Background
import Element.Font
import Element.Input
import Elm.Version
import Html
import Http
import Lamdera
import List.Extra as List
import Task
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , state = Dict.empty
      , order = RequestOrder
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
            , Backend.createPullRequest 1 "test" "MartinSStewart" "elm-serialize" "master"
                |> Task.attempt CreateForkResult
            )

        CreateForkResult result ->
            let
                _ =
                    Debug.log "fork" result
            in
            ( model, Cmd.none )


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
              }
            , Cmd.none
            )


createPullRequestButton =
    Element.Input.button
        buttonAttributes
        { onPress = Just PressedCreateFork
        , label = Element.text "Create pull request"
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
            []
            (Element.column
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
                    ]
                , packagesView model
                ]
            )
        ]
    }


packagesView : FrontendModel -> Element msg
packagesView model =
    Element.column
        []
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
        [ Element.row [ Element.spacing 8 ]
            [ Element.text (packageName ++ " " ++ Elm.Version.toString (Types.packageVersion_ status))
            , case status of
                FetchedAndChecked_ { result } ->
                    case result of
                        NoErrors ->
                            Element.el
                                [ Element.Font.color <| Element.rgb 0.1 0.7 0.1 ]
                                (Element.text "Passed")

                        RuleErrors _ ->
                            Element.el [ errorColor ] (Element.text "Found errors but the default branch doesn't match package tag ")

                        RuleErrorsFromTag _ ->
                            Element.el [ errorColor ] (Element.text "Found errors in package tag but failed to handle default branch")

                        RuleErrorsAndPullRequest _ url ->
                            Element.newTabLink
                                [ Element.Font.color <| Element.rgb 0.1 0.1 0.9 ]
                                { url = url, label = Element.text "Found errors and created PR" }

                        NotAnElm19xPackage ->
                            Element.el [ errorColor ] (Element.text "Not an Elm 19.x package")

                        ParsingError ->
                            Element.el [ errorColor ] (Element.text "Parsing error")

                        IncorrectProject ->
                            Element.el [ errorColor ] (Element.text "Incorrect project")

                        CouldNotOpenDefaultBranchZip ->
                            Element.el [ errorColor ] (Element.text "Could not open branch zip")

                        CouldNotOpenTagZip ->
                            Element.el [ errorColor ] (Element.text "Could not open tag zip")

                        PackageTagNotFound ->
                            Element.el [ errorColor ] (Element.text "Package tag not found")

                        HttpError _ ->
                            Element.el [ errorColor ] (Element.text "Http error")

                        InvalidPackageName ->
                            Element.el [ errorColor ] (Element.text "Invalid package name")

                Fetched_ _ ->
                    Element.text "Fetched"

                FetchingElmJsonAndDocsFailed_ _ _ error ->
                    httpErrorToString error
                        |> (++) "Http error: "
                        |> Element.text
                        |> Element.el [ errorColor ]
            ]
        , case status of
            FetchedAndChecked_ { result } ->
                case result of
                    RuleErrors { errors } ->
                        List.map
                            (\{ ruleName, message } ->
                                Element.paragraph
                                    []
                                    [ Element.text <| ruleName ++ ": " ++ message ]
                            )
                            errors
                            |> Element.column [ errorColor ]

                    RuleErrorsFromTag { errors } ->
                        List.map
                            (\{ ruleName, message } ->
                                Element.paragraph
                                    []
                                    [ Element.text <| ruleName ++ ": " ++ message ]
                            )
                            errors
                            |> Element.column [ errorColor ]

                    RuleErrorsAndPullRequest errors _ ->
                        List.map
                            (\{ ruleName, message } ->
                                Element.paragraph
                                    []
                                    [ Element.text <| ruleName ++ ": " ++ message ]
                            )
                            errors
                            |> Element.column [ errorColor ]

                    HttpError httpError ->
                        Element.el [ errorColor ] (Element.text (httpErrorToString httpError))

                    _ ->
                        Element.none

            _ ->
                Element.none
        ]
    )


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
