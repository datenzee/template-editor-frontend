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
import TemplateEditor.Api.TemplateEditor.TemplateEditors as TemplateEditors
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.Pagination exposing (Pagination)
import TemplateEditor.Data.TemplateEditor exposing (TemplateEditor)


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
            case result of
                Ok data ->
                    ( { model | templateEditors = ActionResult.Success data }, Cmd.none )

                Err error ->
                    ( { model | templateEditors = ActionResult.Error "Unable to get template editors" }, Cmd.none )


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
                    li [] [ text editor.name ]
            in
            div []
                [ h2 [] [ text "Editors" ]
                , ul [] (List.map viewEditor pagination.items)
                ]
