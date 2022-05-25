module TemplateEditor.Pages.Dashboard exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Html exposing (Html, div, h2, li, text, ul)
import Html.Extra exposing (emptyNode)
import Http
import TemplateEditor.Api.TemplateEditor.Data.Pagination exposing (Pagination)
import TemplateEditor.Api.TemplateEditor.Data.TemplateEditor exposing (TemplateEditor)
import TemplateEditor.Api.TemplateEditor.TemplateEditors as TemplateEditors
import TemplateEditor.Common.Setters exposing (setTemplateEditors)
import TemplateEditor.Components.Link exposing (linkTo)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Routes as Routes


type alias Model =
    { templateEditors : ActionResult (Pagination TemplateEditor) }


initialModel : Model
initialModel =
    { templateEditors = ActionResult.Loading }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel
    , TemplateEditors.getTemplateEditors appState GetTemplateEditorsComplete
    )


type Msg
    = GetTemplateEditorsComplete (Result Http.Error (Pagination TemplateEditor))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTemplateEditorsComplete result ->
            ( ActionResult.apply setTemplateEditors
                (always (ActionResult.Error "Unable to get template editors"))
                result
                model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.templateEditors of
        ActionResult.Unset ->
            emptyNode

        ActionResult.Loading ->
            div [] [ text "Loading" ]

        ActionResult.Error error ->
            div [] [ text error ]

        ActionResult.Success pagination ->
            let
                viewEditor editor =
                    li []
                        [ linkTo (Routes.TemplateEditor editor.id) [] [ text editor.name ]
                        ]
            in
            div []
                [ h2 [] [ text "Editors" ]
                , ul [] (List.map viewEditor pagination.items)
                ]
