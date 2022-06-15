module TemplateEditor.Pages.TemplateEditor.Canvas exposing (..)

import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class, style)
import TemplateEditor.Data.DUIO.App exposing (App)
import TemplateEditor.Data.DUIO.Component exposing (Component(..), PlainTextComponent(..))


type alias Model =
    { app : App }


init : App -> Model
init =
    Model


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div [ style "max-width" "60rem" ] [ viewApp model.app ]


viewApp : App -> Html Msg
viewApp app =
    viewCard ( "fa-window-maximize", "App" )
        [ viewComponent app.rootComponent
        ]


viewComponent : Component -> Html Msg
viewComponent component =
    case component of
        ContainerComponent _ ->
            viewCard ( "fa-square", "Container" )
                []

        IterativeContainerComponent _ ->
            viewCard ( "fa-sync-alt", "IterativeContainer" )
                []

        PlainTextComponentComponent plainTextComponent ->
            case plainTextComponent of
                HeadingComponent _ ->
                    viewCard ( "fas fa-heading", "Heading" )
                        []

                ParagraphComponent _ ->
                    viewCard ( "fas fa-paragraph", "Paragraph" )
                        []

        TitleComponentComponent _ ->
            viewCard ( "fa-window-maximize", "Container" )
                []


viewCard : ( String, String ) -> List (Html Msg) -> Html Msg
viewCard ( icon, label ) content =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ i [ class ("far mr-1 " ++ icon) ] []
            , text label
            ]
        , div [ class "card-body" ] content
        ]
