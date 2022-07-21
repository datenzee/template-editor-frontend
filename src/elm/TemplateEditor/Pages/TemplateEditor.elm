module TemplateEditor.Pages.TemplateEditor exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Html exposing (Html, button, div, h2, hr, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Html.Extra exposing (emptyNode)
import Http
import Maybe.Extra as Maybe
import Random exposing (Seed)
import TemplateEditor.Api.TemplateEditor.Data.TemplateEditorDetail exposing (TemplateEditorDetail)
import TemplateEditor.Api.TemplateEditor.TemplateEditors as TemplateEditors
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.DUIO.App as App
import TemplateEditor.Pages.TemplateEditor.Canvas as Canvas
import TemplateEditor.Ports as Ports


type alias Model =
    { id : Int
    , templateEditor : ActionResult TemplateEditorDetail
    , saving : ActionResult ()
    , publishing : ActionResult ()
    , canvasModel : Maybe Canvas.Model
    }


initialModel : Int -> Model
initialModel id =
    { id = id
    , templateEditor = ActionResult.Loading
    , saving = ActionResult.Unset
    , publishing = ActionResult.Unset
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
    | Publish
    | PublishComplete (Result Http.Error ())
    | ResetPublish
    | CanvasMsg Canvas.Msg
    | CopyToClipboard


update : AppState -> Msg -> Model -> ( Seed, Model, Cmd Msg )
update appState msg model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )
    in
    case msg of
        GetTemplateEditorComplete result ->
            let
                setData templateEditor m =
                    { m
                        | templateEditor = templateEditor
                        , canvasModel = ActionResult.toMaybe <| ActionResult.map (Canvas.init << .content) templateEditor
                    }
            in
            withSeed <|
                ( ActionResult.apply setData
                    (always (ActionResult.Error "Unable to get template editor"))
                    result
                    model
                , Cmd.none
                )

        Save ->
            withSeed <|
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
            withSeed <|
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
            withSeed <|
                ( { model | saving = ActionResult.Unset }, Cmd.none )

        Publish ->
            withSeed <|
                case model.templateEditor of
                    ActionResult.Success templateEditor ->
                        let
                            rdf =
                                App.toRdf templateEditor.content

                            cmd =
                                TemplateEditors.publish appState model.id rdf PublishComplete
                        in
                        ( { model | publishing = ActionResult.Loading }
                        , cmd
                        )

                    _ ->
                        ( model, Cmd.none )

        PublishComplete result ->
            withSeed <|
                case result of
                    Ok _ ->
                        ( { model | publishing = ActionResult.Success () }
                        , Cmd.none
                        )

                    Err _ ->
                        ( { model | publishing = ActionResult.Error "Unable to publish" }
                        , Cmd.none
                        )

        ResetPublish ->
            withSeed <|
                ( { model | publishing = ActionResult.Unset }, Cmd.none )

        CanvasMsg canvasMsg ->
            case model.canvasModel of
                Just canvasModel ->
                    let
                        ( seed, newCanvasModel ) =
                            Canvas.update appState canvasMsg canvasModel
                    in
                    ( seed
                    , { model | canvasModel = Just newCanvasModel }
                    , Cmd.none
                    )

                Nothing ->
                    withSeed <| ( model, Cmd.none )

        CopyToClipboard ->
            withSeed <|
                case model.canvasModel of
                    Just canvasModel ->
                        let
                            rdf =
                                App.toRdf canvasModel.app

                            cmd =
                                Ports.copyToClipboard rdf
                        in
                        ( model, cmd )

                    _ ->
                        ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.canvasModel of
        Just canvasModel ->
            Sub.map CanvasMsg (Canvas.subscriptions canvasModel)

        Nothing ->
            Sub.none


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
                savingResult =
                    case model.saving of
                        ActionResult.Success _ ->
                            div [ class "alert alert-success" ]
                                [ text "Saved!"
                                , button [ onClick ResetSave, class "btn btn-success" ] [ text "OK" ]
                                ]

                        ActionResult.Error err ->
                            div [ class "alert alert-danger" ]
                                [ text err
                                , button [ onClick ResetSave, class "btn btn-danger" ] [ text "OK" ]
                                ]

                        _ ->
                            emptyNode

                publishingResult =
                    case model.publishing of
                        ActionResult.Success _ ->
                            div [ class "alert alert-success" ]
                                [ text "Published!"
                                , button [ onClick ResetPublish, class "btn btn-success" ] [ text "OK" ]
                                ]

                        ActionResult.Error err ->
                            div [ class "alert alert-danger" ]
                                [ text err
                                , button [ onClick ResetPublish, class "btn btn-danger" ] [ text "OK" ]
                                ]

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
            div [ class "TemplateEditor" ]
                [ div [ class "d-flex justify-content-between align-items-center" ]
                    [ h2 [] [ text editor.name ]
                    , savingResult
                    , publishingResult
                    , div []
                        [ button
                            [ class "btn btn-outline-primary mr-2"
                            , onClick CopyToClipboard
                            ]
                            [ text "Copy" ]
                        , button
                            [ class "btn btn-outline-primary mr-2"
                            , onClick Publish
                            , disabled (ActionResult.isLoading model.publishing)
                            ]
                            [ text "Publish" ]
                        , button
                            [ class "btn btn-primary"
                            , onClick Save
                            , disabled (ActionResult.isLoading model.saving)
                            ]
                            [ text "Save" ]
                        ]
                    ]
                , hr [] []
                , canvas
                ]
