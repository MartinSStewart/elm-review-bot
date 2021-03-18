module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Element exposing (Element)
import Element.Background
import Element.Font
import Element.Input
import Elm.Version
import Html
import Html.Attributes as Attr
import Http
import Lamdera
import Review.Rule
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
init url key =
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

        UrlChanged url ->
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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        Updates dict ->
            ( { model
                | state =
                    Dict.merge
                        (\key a state -> Dict.insert key a state)
                        (\key a new state -> Dict.insert key (a ++ new) state)
                        (\key new state -> Dict.insert key new state)
                        model.state
                        dict
                        Dict.empty
              }
            , Cmd.none
            )


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
                                , label = Element.text "Show alphabetical"
                                }
                    , Element.text <| "Total packages: " ++ String.fromInt (Dict.size model.state)
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
            |> List.concatMap (\( packageName, versions ) -> List.map (packageView packageName) versions)
            |> (case model.order of
                    RequestOrder ->
                        List.sortBy (\( a, _, _ ) -> Types.packageIndex a)

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
            [ Element.text packageName
            , Element.el
                [ Element.Font.color <| Element.rgb 0.2 0.2 0.2 ]
                (Element.text (Types.packageVersion_ status |> Elm.Version.toString))
            , case status of
                FetchedAndChecked_ { errors } ->
                    case errors of
                        [] ->
                            Element.el
                                [ Element.Font.color <| Element.rgb 0.1 0.7 0.1 ]
                                (Element.text "Passed")

                        _ ->
                            Element.el
                                [ errorColor ]
                                Element.none

                FetchingZipFailed_ _ _ error ->
                    (case error of
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
                    )
                        |> (++) "Http error: "
                        |> Element.text
                        |> Element.el [ errorColor ]
            ]
        , case status of
            FetchedAndChecked_ { errors } ->
                case errors of
                    [] ->
                        Element.none

                    _ ->
                        List.map
                            (\{ ruleName, message } ->
                                Element.paragraph
                                    []
                                    [ Element.text <| ruleName ++ ": " ++ message ]
                            )
                            errors
                            |> Element.column [ errorColor ]

            FetchingZipFailed_ _ _ _ ->
                Element.none
        ]
    )


errorColor =
    Element.Font.color <| Element.rgb 0.8 0.1 0.1
