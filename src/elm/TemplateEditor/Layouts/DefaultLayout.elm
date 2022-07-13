module TemplateEditor.Layouts.DefaultLayout exposing (view)

import Browser exposing (Document)
import Html exposing (Html, a, div, h1, hr, i, span, text)
import Html.Attributes exposing (class)
import Html.Events.Extra exposing (onLinkClick)
import Html.Extra exposing (emptyNode)
import TemplateEditor.Api.DSW.Data.User as User
import TemplateEditor.Components.Link exposing (linkTo)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Routes as Routes


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
                        , span [ class "mx-1" ] [ text " | " ]
                        , a [ onLinkClick cfg.logoutMsg, class "text-white pointer" ] [ text "Logout" ]
                        ]

                Nothing ->
                    emptyNode
    in
    { title = title
    , body =
        [ div [ class "d-flex justify-content-between align-items-center rounded-lg datenzee-gradient text-white m-3 px-3 py-2" ]
            [ h1 []
                [ linkTo Routes.Dashboard
                    []
                    [ i [ class "fas fa-pen-fancy mr-3" ] []
                    , text "Template Editor"
                    ]
                ]
            , userName
            ]
        , div [ class "m-3" ]
            [ content ]
        ]
    }
