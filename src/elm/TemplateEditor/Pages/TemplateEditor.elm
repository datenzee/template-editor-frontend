module TemplateEditor.Pages.TemplateEditor exposing (Model, Msg, init, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, div, h2, li, text)
import Html.Extra exposing (emptyNode)
import Http
import TemplateEditor.Api.TemplateEditor.Data.TemplateEditorDetail exposing (TemplateEditorDetail)
import TemplateEditor.Api.TemplateEditor.TemplateEditors as TemplateEditors
import TemplateEditor.Common.Setters exposing (setTemplateEditor)
import TemplateEditor.Data.AppState exposing (AppState)
import Uuid exposing (Uuid)


type alias Model =
    { templateEditor : ActionResult TemplateEditorDetail }


initialModel : Model
initialModel =
    { templateEditor = ActionResult.Loading }


init : AppState -> Int -> ( Model, Cmd Msg )
init appState id =
    ( initialModel
    , TemplateEditors.getTemplateEditor appState id GetTemplateEditorComplete
    )


type Msg
    = GetTemplateEditorComplete (Result Http.Error TemplateEditorDetail)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTemplateEditorComplete result ->
            ( ActionResult.apply setTemplateEditor
                (always (ActionResult.Error "Unable to get template editor"))
                result
                model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.templateEditor of
        ActionResult.Unset ->
            emptyNode

        ActionResult.Loading ->
            div [] [ text "Loading" ]

        ActionResult.Error error ->
            div [] [ text error ]

        ActionResult.Success editor ->
            div []
                [ h2 [] [ text editor.name ]
                ]
