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
import Html exposing (Html, a, button, div, h2, hr, iframe, input, label, li, text, textarea, ul)
import Html.Attributes exposing (class, classList, disabled, readonly, src, value)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (emptyNode)
import Http
import Maybe.Extra as Maybe
import Random exposing (Seed)
import TemplateEditor.Api.TemplateEditor.Data.PublishResult exposing (PublishResult)
import TemplateEditor.Api.TemplateEditor.Data.TemplateEditorDetail exposing (TemplateEditorDetail)
import TemplateEditor.Api.TemplateEditor.TemplateEditors as TemplateEditors
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.ViewOntology.App as App exposing (ToRdfConfig)
import TemplateEditor.Pages.TemplateEditor.Canvas as Canvas
import TemplateEditor.Ports as Ports


type alias Model =
    { id : Int
    , templateEditor : ActionResult TemplateEditorDetail
    , saving : ActionResult ()
    , publishing : ActionResult ()
    , canvasModel : Maybe Canvas.Model
    , rightView : RightView
    , previewUrl : ActionResult String
    }


getToRdfConfig : AppState -> Model -> ToRdfConfig
getToRdfConfig appState model =
    { basePrefix = Maybe.withDefault "" (ActionResult.unwrap Nothing .baseUrl model.templateEditor)
    , license = Maybe.withDefault "" (ActionResult.unwrap Nothing .license model.templateEditor)
    , time = appState.currentTime
    }


type RightView
    = PreviewView
    | RDFView


initialModel : Int -> Model
initialModel id =
    { id = id
    , templateEditor = ActionResult.Loading
    , saving = ActionResult.Unset
    , publishing = ActionResult.Unset
    , canvasModel = Nothing
    , rightView = RDFView
    , previewUrl = ActionResult.Unset
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
    | PublishComplete (Result Http.Error PublishResult)
    | ResetPublish
    | CanvasMsg Canvas.Msg
    | CopyToClipboard
    | SetRightView RightView
    | UpdateDataUrl String
    | UpdateRootComponent String
    | UpdateExpanderType String
    | UpdateBaseUrl String
    | UpdateLicense String


update : AppState -> Msg -> Model -> ( Seed, Model, Cmd Msg )
update appState msg model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )

        updateValue newValue setValue =
            withSeed <|
                case model.templateEditor of
                    ActionResult.Success templateEditor ->
                        let
                            value =
                                if String.isEmpty newValue then
                                    Nothing

                                else
                                    Just newValue
                        in
                        ( { model | templateEditor = ActionResult.Success (setValue templateEditor value) }
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )
    in
    case msg of
        GetTemplateEditorComplete result ->
            let
                setData templateEditor m =
                    { m
                        | templateEditor = templateEditor
                        , canvasModel = ActionResult.toMaybe <| ActionResult.map (Canvas.init << .content) templateEditor
                        , previewUrl =
                            case templateEditor of
                                ActionResult.Success te ->
                                    Maybe.unwrap ActionResult.Unset ActionResult.Success te.url

                                _ ->
                                    ActionResult.Unset
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
                let
                    rdf =
                        Maybe.unwrap "" (\cm -> App.toRdf cm.app (getToRdfConfig appState model)) model.canvasModel

                    cmd =
                        TemplateEditors.publish appState model.id rdf PublishComplete
                in
                ( { model
                    | publishing = ActionResult.Loading
                    , previewUrl = ActionResult.Loading
                  }
                , cmd
                )

        PublishComplete result ->
            withSeed <|
                case result of
                    Ok publishResult ->
                        ( { model
                            | publishing = ActionResult.Success ()
                            , previewUrl = ActionResult.Success publishResult.url
                          }
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
                        ( seed, newCanvasModel, cmd ) =
                            Canvas.update appState canvasMsg canvasModel
                    in
                    ( seed
                    , { model | canvasModel = Just newCanvasModel }
                    , Cmd.map CanvasMsg cmd
                    )

                Nothing ->
                    withSeed <| ( model, Cmd.none )

        CopyToClipboard ->
            withSeed <|
                case model.canvasModel of
                    Just canvasModel ->
                        let
                            rdf =
                                App.toRdf canvasModel.app (getToRdfConfig appState model)

                            cmd =
                                Ports.copyToClipboard rdf
                        in
                        ( model, cmd )

                    _ ->
                        ( model, Cmd.none )

        SetRightView rightView ->
            withSeed ( { model | rightView = rightView }, Cmd.none )

        UpdateDataUrl newDataUrl ->
            updateValue newDataUrl (\te v -> { te | dataUrl = v })

        UpdateRootComponent newRootComponent ->
            updateValue newRootComponent (\te v -> { te | rootComponent = v })

        UpdateExpanderType newExpanderType ->
            updateValue newExpanderType (\te v -> { te | expanderType = v })

        UpdateBaseUrl newBaseUrl ->
            updateValue newBaseUrl (\te v -> { te | baseUrl = v })

        UpdateLicense newLicense ->
            updateValue newLicense (\te v -> { te | license = v })


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.canvasModel of
        Just canvasModel ->
            Sub.map CanvasMsg (Canvas.subscriptions canvasModel)

        Nothing ->
            Sub.none


view : AppState -> Model -> Html Msg
view appState model =
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

                rdfView =
                    case model.canvasModel of
                        Just canvasModel ->
                            viewRDF appState model canvasModel

                        Nothing ->
                            emptyNode

                preview =
                    case model.templateEditor of
                        ActionResult.Success templateEditor ->
                            viewPreview model

                        _ ->
                            emptyNode

                rightView =
                    case model.rightView of
                        PreviewView ->
                            preview

                        RDFView ->
                            rdfView
            in
            div [ class "TemplateEditor" ]
                [ div [ class "TemplateEditor__Header d-flex justify-content-between align-items-center" ]
                    [ h2 [] [ text editor.name ]
                    , savingResult
                    , publishingResult
                    , div []
                        [ button
                            [ class "btn btn-outline-primary me-2"
                            , onClick CopyToClipboard
                            ]
                            [ text "Copy RDF" ]
                        , button
                            [ class "btn btn-outline-primary me-2"
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
                , div [ class "row" ]
                    [ div [ class "col-6" ]
                        [ div [ class "form-group row mb-1" ]
                            [ label [ class "col-md-2 col-form-label" ] [ text "Data URL" ]
                            , div [ class "col-md-10" ] [ input [ class "form-control", value (Maybe.withDefault "" editor.dataUrl), onInput UpdateDataUrl ] [] ]
                            ]
                        , div [ class "form-group row mb-1" ]
                            [ label [ class "col-md-2 col-form-label" ] [ text "Root Component" ]
                            , div [ class "col-md-10" ] [ input [ class "form-control", value (Maybe.withDefault "" editor.rootComponent), onInput UpdateRootComponent ] [] ]
                            ]
                        , div [ class "form-group row mb-1" ]
                            [ label [ class "col-md-2 col-form-label" ] [ text "Expander Type" ]
                            , div [ class "col-md-10" ] [ input [ class "form-control", value (Maybe.withDefault "" editor.expanderType), onInput UpdateExpanderType ] [] ]
                            ]
                        , div [ class "form-group row mb-1" ]
                            [ label [ class "col-md-2 col-form-label" ] [ text "Base URL" ]
                            , div [ class "col-md-10" ] [ input [ class "form-control", value (Maybe.withDefault "" editor.baseUrl), onInput UpdateBaseUrl ] [] ]
                            ]
                        , div [ class "form-group row mb-3" ]
                            [ label [ class "col-md-2 col-form-label" ] [ text "License" ]
                            , div [ class "col-md-10" ] [ input [ class "form-control", value (Maybe.withDefault "" editor.license), onInput UpdateLicense ] [] ]
                            ]
                        , canvas
                        ]
                    , div [ class "col-6" ]
                        [ ul [ class "nav nav-pills" ]
                            [ li [ class "nav-item" ]
                                [ a
                                    [ class "nav-link"
                                    , classList [ ( "active", model.rightView == PreviewView ) ]
                                    , onClick (SetRightView PreviewView)
                                    ]
                                    [ text "Preview" ]
                                ]
                            , li [ class "nav-item" ]
                                [ a
                                    [ class "nav-link"
                                    , classList [ ( "active", model.rightView == RDFView ) ]
                                    , onClick (SetRightView RDFView)
                                    ]
                                    [ text "RDF" ]
                                ]
                            ]
                        , rightView
                        ]
                    ]
                ]


viewPreview : Model -> Html Msg
viewPreview model =
    case model.previewUrl of
        ActionResult.Success url ->
            let
                iframeUrl =
                    case ActionResult.withDefault Nothing (ActionResult.map .dataUrl model.templateEditor) of
                        Just dataUrl ->
                            url ++ "?data=" ++ dataUrl

                        Nothing ->
                            url
            in
            iframe [ class "app-preview", src iframeUrl ] []

        ActionResult.Loading ->
            div [ class "bookshelf_wrapper" ]
                [ ul [ class "books_list" ]
                    [ li [ class "book_item first" ] []
                    , li [ class "book_item second" ] []
                    , li [ class "book_item third" ] []
                    , li [ class "book_item fourth" ] []
                    , li [ class "book_item fifth" ] []
                    , li [ class "book_item sixth" ] []
                    ]
                , div [ class "shelf" ] []
                ]

        ActionResult.Error error ->
            div [ class "alert alert-danger" ] [ text error ]

        ActionResult.Unset ->
            div [ class "alert alert-info" ] [ text "There is no preview URL yet." ]


viewRDF : AppState -> Model -> Canvas.Model -> Html Msg
viewRDF appState model canvasModel =
    let
        content =
            App.toRdf canvasModel.app (getToRdfConfig appState model)
    in
    textarea [ class "form-control code-preview", readonly True ]
        [ text content ]
