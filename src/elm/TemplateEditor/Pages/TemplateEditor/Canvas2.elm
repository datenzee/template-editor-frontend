module TemplateEditor.Pages.TemplateEditor.Canvas2 exposing (..)

import Html exposing (Html, a, button, div, form, h5, hr, i, input, label, li, pre, span, strong, text, textarea, ul)
import Html.Attributes exposing (checked, class, classList, readonly, rows, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Extra exposing (emptyNode)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import TemplateEditor.Common.FontAwesome exposing (fas)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.ViewOntology.App as App exposing (App)
import TemplateEditor.Data.ViewOntology.Component exposing (DataComponent)


type alias Model =
    { app : App
    , addComponentModalOpen : Bool
    , addComponentModalValue : String
    , addComponentModalError : Maybe String
    , openedDataComponent : Maybe DataComponent
    , dataComponentToDelete : Maybe DataComponent
    }


init : App -> Model
init app =
    { app = app
    , addComponentModalOpen = False
    , addComponentModalValue = ""
    , addComponentModalError = Nothing
    , openedDataComponent = List.head app.components
    , dataComponentToDelete = Nothing
    }


type Msg
    = CollapseAll
    | ExpandAll
    | OpenAddDataComponentModal Bool
    | AddDataComponentModalInput String
    | AddDataComponent
    | OpenDataComponent DataComponent
    | SetDataComponentToDelete (Maybe DataComponent)
    | DeleteDataComponent String


update : AppState -> Msg -> Model -> ( Seed, Model, Cmd Msg )
update appState msg model =
    let
        wrap m =
            ( appState.seed, m, Cmd.none )
    in
    case msg of
        OpenAddDataComponentModal open ->
            wrap
                { model
                    | addComponentModalOpen = open
                    , addComponentModalValue = ""
                    , addComponentModalError = Nothing
                }

        AddDataComponentModalInput value ->
            wrap { model | addComponentModalValue = value }

        AddDataComponent ->
            if App.componentExists model.addComponentModalValue model.app then
                wrap
                    { model
                        | addComponentModalError = Just "Component with this name already exists"
                    }

            else
                let
                    newApp =
                        App.addDataComponent model.addComponentModalValue model.app
                in
                wrap
                    { model
                        | addComponentModalOpen = False
                        , app = newApp
                        , openedDataComponent = List.last newApp.components
                    }

        OpenDataComponent dataComponent ->
            wrap { model | openedDataComponent = Just dataComponent }

        SetDataComponentToDelete dataComponent ->
            wrap { model | dataComponentToDelete = dataComponent }

        DeleteDataComponent name ->
            wrap
                { model
                    | app = App.deleteDataComponent name model.app
                    , openedDataComponent = Nothing
                    , dataComponentToDelete = Nothing
                }

        _ ->
            ( appState.seed, model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        content =
            case model.openedDataComponent of
                Just dataComponent ->
                    div [ class "component-data" ]
                        [ div [] [ strong [] [ text dataComponent.name ] ]
                        , div [ class "mb-3 d-flex justify-content-between" ]
                            [ div []
                                [ a [ onClick ExpandAll, class "text-primary me-2" ] [ text "Expand all" ]
                                , a [ onClick CollapseAll, class "text-primary" ] [ text "Collapse all" ]
                                ]
                            , div []
                                [ a
                                    [ onClick (SetDataComponentToDelete (Just dataComponent))
                                    , class "text-danger"
                                    ]
                                    [ fas "fa-trash" [ class "me-1" ]
                                    , text "Delete"
                                    ]
                                ]
                            ]
                        , viewApp model
                        ]

                Nothing ->
                    div [ class "component-data" ]
                        [ div [ class "alert alert-info" ]
                            [ fas "fa-arrow-left" [ class "me-2" ]
                            , text "Open a component or add a new one"
                            ]
                        ]
    in
    div [ class "app-canvas" ]
        [ viewComponentList model
        , content
        , viewAddDataComponentModal model
        , viewDeleteModal model
        ]


viewComponentList : Model -> Html Msg
viewComponentList model =
    let
        viewComponent component =
            li []
                [ a [ onClick (OpenDataComponent component) ]
                    [ span [ class "fa-li" ] [ fas "fa-file-invoice" [] ]
                    , text component.name
                    ]
                ]
    in
    div [ class "component-list" ]
        [ strong [] [ text "Components" ]
        , ul [ class "fa-ul mt-2" ]
            (List.map viewComponent (List.sortBy .name model.app.components)
                ++ [ li [ class "mt-3" ]
                        [ a [ onClick (OpenAddDataComponentModal True) ]
                            [ span [ class "fa-li" ] [ fas "fa-plus" [] ]
                            , text "Add"
                            ]
                        ]
                   ]
            )
        ]


viewApp : Model -> Html Msg
viewApp model =
    div [] []


viewDataComponent : DataComponent -> Html Msg
viewDataComponent dataComponent =
    div [] []


viewAddDataComponentModal : Model -> Html Msg
viewAddDataComponentModal model =
    let
        errorBlock =
            case model.addComponentModalError of
                Just error ->
                    div [ class "text-danger mb-3" ] [ text error ]

                Nothing ->
                    emptyNode
    in
    div [ class "modal fade", classList [ ( "show", model.addComponentModalOpen ) ] ]
        [ div [ class "modal-dialog modal-dialog-centered" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text "New Component" ] ]
                , div [ class "modal-body" ]
                    [ errorBlock
                    , label [] [ text "Name" ]
                    , div []
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , onInput AddDataComponentModalInput
                            , value model.addComponentModalValue
                            ]
                            []
                        ]
                    ]
                , div [ class "modal-footer justify-content-between" ]
                    [ button [ class "btn btn-secondary", onClick (OpenAddDataComponentModal False) ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick AddDataComponent ] [ text "Add" ]
                    ]
                ]
            ]
        ]


viewDeleteModal : Model -> Html Msg
viewDeleteModal model =
    let
        componentName =
            Maybe.unwrap "" .name model.dataComponentToDelete
    in
    div [ class "modal fade", classList [ ( "show", Maybe.isJust model.dataComponentToDelete ) ] ]
        [ div [ class "modal-dialog modal-dialog-centered" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text "Delete Component" ] ]
                , div [ class "modal-body" ]
                    [ text "Are you sure you want to delete "
                    , strong [] [ text componentName ]
                    , text "?"
                    ]
                , div [ class "modal-footer justify-content-between" ]
                    [ button [ class "btn btn-secondary", onClick (SetDataComponentToDelete Nothing) ] [ text "Cancel" ]
                    , button [ class "btn btn-danger", onClick (DeleteDataComponent componentName) ] [ text "Delete" ]
                    ]
                ]
            ]
        ]
