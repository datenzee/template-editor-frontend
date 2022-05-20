module TemplateEditor.Layouts.DefaultLayout exposing (view)

import Browser exposing (Document)
import Html exposing (Html, a, div, h1, hr, i, text)
import Html.Attributes exposing (class)
import Html.Events.Extra exposing (onLinkClick)
import Html.Extra exposing (emptyNode)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.User as User


type alias ViewConfig msg =
    { title : Maybe String
    , logoutMsg : msg
    }


view : AppState -> ViewConfig msg -> Html msg -> Document msg
view appState cfg content =
    let
        title =
            case cfg.title of
                Just t ->
                    t ++ " | Template Editor"

                Nothing ->
                    "Template Editor"

        userName =
            case appState.session.user of
                Just user ->
                    div []
                        [ text (User.fullName user)
                        , a [ onLinkClick cfg.logoutMsg, class "text-white ml-2 pointer" ] [ text "Logout" ]
                        ]

                Nothing ->
                    emptyNode
    in
    { title = title
    , body =
        [ div [ class "jumbotron datenzee-gradient text-white m-3" ]
            [ h1 []
                [ i [ class "fas fa-pen-fancy mr-3" ] []
                , text "Template Editor"
                ]
            , userName
            ]
        , div [ class "m-3" ]
            [ content ]
        ]
    }
