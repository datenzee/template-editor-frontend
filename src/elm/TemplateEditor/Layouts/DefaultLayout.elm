module TemplateEditor.Layouts.DefaultLayout exposing (view)

import Browser exposing (Document)
import Html exposing (Html, div, h1, hr, i, text)
import Html.Attributes exposing (class)
import Html.Extra exposing (emptyNode)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.User as User


view : AppState -> Maybe String -> Html msg -> Document msg
view appState mbTitle content =
    let
        title =
            case mbTitle of
                Just t ->
                    t ++ " | Template Editor"

                Nothing ->
                    "Template Editor"

        userName =
            case appState.session.user of
                Just user ->
                    text (User.fullName user)

                Nothing ->
                    emptyNode
    in
    { title = title
    , body =
        [ div [ class "jumbotron m-3" ]
            [ h1 []
                [ i [ class "fas fa-pen-fancy mr-3" ] []
                , text "Template Editor"
                ]
            , userName
            , hr [] []
            , content
            ]
        ]
    }
