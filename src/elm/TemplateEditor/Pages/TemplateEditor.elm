module TemplateEditor.Pages.TemplateEditor exposing (Model, Msg, init, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, a, button, div, h2, hr, li, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra exposing (emptyNode)
import Http
import Maybe.Extra as Maybe
import TemplateEditor.Api.TemplateEditor.Data.TemplateEditorDetail exposing (TemplateEditorDetail)
import TemplateEditor.Api.TemplateEditor.TemplateEditors as TemplateEditors
import TemplateEditor.Common.Setters exposing (setTemplateEditor)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Pages.TemplateEditor.Canvas as Canvas
import Uuid exposing (Uuid)


type alias Model =
    { id : Int
    , templateEditor : ActionResult TemplateEditorDetail
    , saving : ActionResult ()
    , canvasModel : Maybe Canvas.Model
    }


initialModel : Int -> Model
initialModel id =
    { id = id
    , templateEditor = ActionResult.Loading
    , saving = ActionResult.Unset
    , canvasModel = Nothing
    }


init : AppState -> Int -> ( Model, Cmd Msg )
init appState id =
    ( initialModel id
    , TemplateEditors.getTemplateEditor appState id GetTemplateEditorComplete
    )


type Msg
    = GetTemplateEditorComplete (Result Http.Error TemplateEditorDetail)
    | Save
    | SaveComplete (Result Http.Error ())
    | ResetSave
    | CanvasMsg Canvas.Msg


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetTemplateEditorComplete result ->
            let
                setData templateEditor m =
                    { m
                        | templateEditor = templateEditor
                        , canvasModel = ActionResult.toMaybe <| ActionResult.map (Canvas.init << .content) templateEditor
                    }
            in
            ( ActionResult.apply setData
                (always (ActionResult.Error "Unable to get template editor"))
                result
                model
            , Cmd.none
            )

        Save ->
            case model.templateEditor of
                ActionResult.Success templateEditor ->
                    let
                        newTemplateEditor =
                            { templateEditor | content = Maybe.unwrap templateEditor.content .app model.canvasModel }

                        cmd =
                            TemplateEditors.putTemplateEditor appState model.id newTemplateEditor SaveComplete
                    in
                    ( { model | saving = ActionResult.Loading }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        SaveComplete result ->
            case result of
                Ok _ ->
                    ( { model | saving = ActionResult.Success () }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | saving = ActionResult.Error "Unable to save data" }
                    , Cmd.none
                    )

        ResetSave ->
            ( { model | saving = ActionResult.Unset }, Cmd.none )

        CanvasMsg canvasMsg ->
            case model.canvasModel of
                Just canvasModel ->
                    ( { model | canvasModel = Just <| Canvas.update canvasMsg canvasModel }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


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
            let
                savingError =
                    case model.saving of
                        ActionResult.Success _ ->
                            div [ class "alert alert-success" ]
                                [ text "Saved!"
                                , button [ onClick ResetSave, class "btn btn-success" ] [ text "OK" ]
                                ]

                        ActionResult.Error err ->
                            div [ class "alert alert-danger" ] [ text err ]

                        _ ->
                            emptyNode

                canvas =
                    case model.canvasModel of
                        Just canvasModel ->
                            Html.map CanvasMsg <|
                                Canvas.view canvasModel

                        Nothing ->
                            emptyNode
            in
            div []
                [ h2 [] [ text editor.name ]
                , savingError
                , button [ class "btn btn-secondary", onClick Save ] [ text "Save" ]
                , hr [] []
                , canvas
                ]
