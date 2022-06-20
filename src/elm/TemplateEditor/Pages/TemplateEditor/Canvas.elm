module TemplateEditor.Pages.TemplateEditor.Canvas exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Dict exposing (Dict)
import Html exposing (Html, a, div, form, hr, i, input, label, pre, text, textarea)
import Html.Attributes exposing (class, readonly, rows, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Extra exposing (emptyNode)
import Maybe.Extra as Maybe
import Random exposing (Seed)
import TemplateEditor.Common.FontAwesome exposing (fa, far, fas)
import TemplateEditor.Common.Setters exposing (setContent, setPredicate)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.DUIO.App as App exposing (App)
import TemplateEditor.Data.DUIO.Component as Component exposing (Component(..), Container, IterativeContainer, PlainTextComponent(..), PlainTextComponentData, TitleComponent)
import Uuid exposing (Uuid)


type alias Model =
    { app : App
    , dropdownStates : Dict String Dropdown.State
    }


init : App -> Model
init app =
    { app = app
    , dropdownStates = Dict.empty
    }


type Msg
    = DropdownMsg String Dropdown.State
    | AddContainer Uuid
    | AddIterativeContainer Uuid
    | AddTitleComponent Uuid
    | AddHeadingComponent Uuid
    | AddParagraphComponent Uuid
    | UpdateIterativeContainerPredicate Uuid String
    | UpdateTitleComponentPredicate Uuid String
    | UpdatePlainTextComponentContent Uuid String
    | MoveComponentUp Uuid
    | MoveComponentDown Uuid
    | DeleteComponent Uuid


update : AppState -> Msg -> Model -> ( Seed, Model )
update appState msg model =
    let
        updateRootComponent rootComponent =
            let
                app =
                    model.app
            in
            { model | app = { app | rootComponent = rootComponent } }

        addComponent parentUuid component =
            let
                insert : Container -> Container
                insert parentContainer =
                    { parentContainer | contains = parentContainer.contains ++ [ component ] }
            in
            updateRootComponent <|
                mapContainer (mapHelper parentUuid insert) model.app.rootComponent

        updateComponent uuid mapFn updateFn =
            updateRootComponent <|
                mapFn (mapHelper uuid updateFn) model.app.rootComponent
    in
    case msg of
        DropdownMsg uuid state ->
            ( appState.seed
            , { model | dropdownStates = Dict.insert uuid state model.dropdownStates }
            )

        AddContainer parentUuid ->
            let
                ( uuid, newSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                container =
                    ContainerComponent
                        { uuid = uuid
                        , contains = []
                        }
            in
            ( newSeed, addComponent parentUuid container )

        AddIterativeContainer parentUuid ->
            let
                ( uuid1, newSeed1 ) =
                    Random.step Uuid.uuidGenerator appState.seed

                ( uuid2, newSeed2 ) =
                    Random.step Uuid.uuidGenerator newSeed1

                iterativeContainer =
                    IterativeContainerComponent
                        { uuid = uuid1
                        , predicate = ""
                        , content =
                            { uuid = uuid2
                            , contains = []
                            }
                        }
            in
            ( newSeed2, addComponent parentUuid iterativeContainer )

        AddTitleComponent parentUuid ->
            let
                ( uuid, newSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                titleComponent =
                    TitleComponentComponent
                        { uuid = uuid
                        , predicate = ""
                        }
            in
            ( newSeed, addComponent parentUuid titleComponent )

        AddHeadingComponent parentUuid ->
            let
                ( uuid, newSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                headingComponent =
                    PlainTextComponentComponent <|
                        HeadingComponent
                            { uuid = uuid
                            , content = ""
                            }
            in
            ( newSeed, addComponent parentUuid headingComponent )

        AddParagraphComponent parentUuid ->
            let
                ( uuid, newSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                paragraphComponent =
                    PlainTextComponentComponent <|
                        ParagraphComponent
                            { uuid = uuid
                            , content = ""
                            }
            in
            ( newSeed, addComponent parentUuid paragraphComponent )

        UpdateIterativeContainerPredicate uuid predicate ->
            ( appState.seed
            , updateComponent uuid mapIterativeContainer (setPredicate predicate)
            )

        UpdateTitleComponentPredicate uuid predicate ->
            ( appState.seed
            , updateComponent uuid mapTitleComponent (setPredicate predicate)
            )

        UpdatePlainTextComponentContent uuid content ->
            ( appState.seed
            , updateComponent uuid mapPlainTextComponent (setContent content)
            )

        MoveComponentUp uuid ->
            let
                move : Container -> Container
                move container =
                    { container | contains = moveUp uuid container.contains }

                rootComponent =
                    mapContainer move model.app.rootComponent
            in
            ( appState.seed, updateRootComponent rootComponent )

        MoveComponentDown uuid ->
            let
                move : Container -> Container
                move container =
                    { container | contains = moveDown uuid container.contains }

                rootComponent =
                    mapContainer move model.app.rootComponent
            in
            ( appState.seed, updateRootComponent rootComponent )

        DeleteComponent uuid ->
            let
                delete : Container -> Container
                delete container =
                    { container | contains = List.filter ((/=) uuid << Component.getUuid) container.contains }

                rootComponent =
                    mapContainer delete model.app.rootComponent
            in
            ( appState.seed, updateRootComponent rootComponent )


mapContainer : (Container -> Container) -> Component -> Component
mapContainer map component =
    case component of
        ContainerComponent container ->
            ContainerComponent <|
                map { container | contains = List.map (mapContainer map) container.contains }

        IterativeContainerComponent iterativeContainer ->
            IterativeContainerComponent { iterativeContainer | content = map iterativeContainer.content }

        other ->
            other


mapIterativeContainer : (IterativeContainer -> IterativeContainer) -> Component -> Component
mapIterativeContainer map component =
    let
        mapContainer_ container =
            { container | contains = List.map (mapIterativeContainer map) container.contains }
    in
    case component of
        ContainerComponent container ->
            ContainerComponent <| mapContainer_ container

        IterativeContainerComponent iterativeContainer ->
            IterativeContainerComponent <|
                map { iterativeContainer | content = mapContainer_ iterativeContainer.content }

        other ->
            other


mapTitleComponent : (TitleComponent -> TitleComponent) -> Component -> Component
mapTitleComponent map component =
    let
        mapContainer_ container =
            { container | contains = List.map (mapTitleComponent map) container.contains }
    in
    case component of
        ContainerComponent container ->
            ContainerComponent <| mapContainer_ container

        IterativeContainerComponent iterativeContainer ->
            IterativeContainerComponent <|
                { iterativeContainer | content = mapContainer_ iterativeContainer.content }

        TitleComponentComponent titleComponent ->
            TitleComponentComponent <|
                map titleComponent

        other ->
            other


mapPlainTextComponent : (PlainTextComponentData -> PlainTextComponentData) -> Component -> Component
mapPlainTextComponent map component =
    let
        mapContainer_ container =
            { container | contains = List.map (mapPlainTextComponent map) container.contains }
    in
    case component of
        ContainerComponent container ->
            ContainerComponent <| mapContainer_ container

        IterativeContainerComponent iterativeContainer ->
            IterativeContainerComponent <|
                { iterativeContainer | content = mapContainer_ iterativeContainer.content }

        PlainTextComponentComponent plainTextComponent ->
            PlainTextComponentComponent <|
                case plainTextComponent of
                    HeadingComponent data ->
                        HeadingComponent <| map data

                    ParagraphComponent data ->
                        ParagraphComponent <| map data

        other ->
            other


mapHelper : Uuid -> ({ a | uuid : Uuid } -> { a | uuid : Uuid }) -> { a | uuid : Uuid } -> { a | uuid : Uuid }
mapHelper uuid map container =
    if container.uuid == uuid then
        map container

    else
        container


moveUp : Uuid -> List Component -> List Component
moveUp uuid components =
    let
        fold current acc =
            let
                currentUuid =
                    Component.getUuid current
            in
            if currentUuid == uuid then
                List.take (List.length acc - 1) acc ++ [ current ] ++ List.drop (List.length acc - 1) acc

            else
                acc ++ [ current ]
    in
    List.foldl fold [] components


moveDown : Uuid -> List Component -> List Component
moveDown uuid components =
    let
        fold current { swapped, acc } =
            let
                length =
                    List.length acc - 1

                start =
                    List.take length acc

                end =
                    List.drop length acc

                lastUuid =
                    List.head end
                        |> Maybe.unwrap Uuid.nil Component.getUuid
            in
            if not swapped && lastUuid == uuid then
                { swapped = True, acc = start ++ [ current ] ++ end }

            else
                { swapped = swapped, acc = acc ++ [ current ] }
    in
    (List.foldl fold { swapped = False, acc = [] } components).acc


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        toDropdownSub ( uuid, state ) =
            Dropdown.subscriptions state (DropdownMsg uuid)
    in
    Dict.toList model.dropdownStates
        |> List.map toDropdownSub
        |> Sub.batch


view : Model -> Html Msg
view model =
    div [ class "app-canvas row" ]
        [ div [ class "col-6" ] [ viewApp model ]
        , div [ class "col-6" ] [ viewCode model ]
        ]


viewCode : Model -> Html Msg
viewCode model =
    let
        content =
            App.toRdf model.app
    in
    textarea [ class "form-control", readonly True ]
        [ text content ]


viewApp : Model -> Html Msg
viewApp model =
    let
        rootContainer =
            case model.app.rootComponent of
                ContainerComponent container ->
                    viewContainer model True container

                _ ->
                    viewComponent model model.app.rootComponent
    in
    viewCard { icon = "far fa-window-maximize", label = "App", uuid = Uuid.nil, controls = False }
        [ rootContainer ]


viewComponent : Model -> Component -> Html Msg
viewComponent model component =
    case component of
        ContainerComponent container ->
            viewContainer model False container

        IterativeContainerComponent iterativeContainer ->
            viewCard { icon = "fas fa-sync-alt", label = "IterativeContainer", uuid = iterativeContainer.uuid, controls = True }
                [ div [ class "form-group row" ]
                    [ label
                        [ class "col-md-2 col-form-label"
                        ]
                        [ text "Predicate" ]
                    , div [ class "col-md-10" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , onInput (UpdateIterativeContainerPredicate iterativeContainer.uuid)
                            , value iterativeContainer.predicate
                            ]
                            []
                        ]
                    ]
                , hr [] []
                , viewContainer model True iterativeContainer.content
                ]

        PlainTextComponentComponent plainTextComponent ->
            case plainTextComponent of
                HeadingComponent headingComponent ->
                    viewCard { icon = "fas fa-heading", label = "Heading", uuid = headingComponent.uuid, controls = True }
                        [ div [ class "form-group row" ]
                            [ label
                                [ class "col-md-2 col-form-label"
                                ]
                                [ text "Content" ]
                            , div [ class "col-md-10" ]
                                [ input
                                    [ type_ "text"
                                    , class "form-control"
                                    , onInput (UpdatePlainTextComponentContent headingComponent.uuid)
                                    , value headingComponent.content
                                    ]
                                    []
                                ]
                            ]
                        ]

                ParagraphComponent paragraphComponent ->
                    viewCard { icon = "fas fa-paragraph", label = "Paragraph", uuid = paragraphComponent.uuid, controls = True }
                        [ div [ class "form-group" ]
                            [ label [ class "form-label" ]
                                [ text "Content" ]
                            , textarea
                                [ class "form-control"
                                , onInput (UpdatePlainTextComponentContent paragraphComponent.uuid)
                                , value paragraphComponent.content
                                ]
                                []
                            ]
                        ]

        TitleComponentComponent titleComponent ->
            viewCard { icon = "fas fa-link", label = "Title", uuid = titleComponent.uuid, controls = True }
                [ div [ class "form-group row" ]
                    [ label
                        [ class "col-md-2 col-form-label"
                        ]
                        [ text "Predicate" ]
                    , div [ class "col-md-10" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , onInput (UpdateTitleComponentPredicate titleComponent.uuid)
                            , value titleComponent.predicate
                            ]
                            []
                        ]
                    ]
                ]


viewContainer : Model -> Bool -> Container -> Html Msg
viewContainer model contentOnly container =
    let
        uuidString =
            Uuid.toString container.uuid

        children =
            List.map (viewComponent model) container.contains

        dropdownState =
            Maybe.withDefault Dropdown.initialState <|
                Dict.get uuidString model.dropdownStates

        dropdownItem icon label msg =
            Dropdown.buttonItem [ onClick msg ] [ fa icon [ class "mr-2 text-muted" ], text label ]

        dropdown =
            Dropdown.dropdown dropdownState
                { options = []
                , toggleMsg = DropdownMsg uuidString
                , toggleButton =
                    Dropdown.toggle [ Button.outlineSecondary, Button.small ]
                        [ fas "fa-plus" [ class "mr-1" ]
                        , text "Add"
                        ]
                , items =
                    [ dropdownItem "far fa-square" "Container" (AddContainer container.uuid)
                    , dropdownItem "fas fa-sync-alt" "IterativeContainer" (AddIterativeContainer container.uuid)
                    , dropdownItem "fas fa-link" "Title" (AddTitleComponent container.uuid)
                    , dropdownItem "fas fa-heading" "PlainText / Heading" (AddHeadingComponent container.uuid)
                    , dropdownItem "fas fa-paragraph" "PlainText / Paragraph" (AddParagraphComponent container.uuid)
                    ]
                }

        wrapper =
            if contentOnly then
                div []

            else
                viewCard { icon = "far fa-square", label = "Container", uuid = container.uuid, controls = True }
    in
    wrapper (children ++ [ dropdown ])


type alias ViewCardConfig =
    { icon : String
    , label : String
    , uuid : Uuid
    , controls : Bool
    }


viewCard : ViewCardConfig -> List (Html Msg) -> Html Msg
viewCard { icon, label, uuid, controls } content =
    let
        moveUpButton =
            a [ onClick (MoveComponentUp uuid), class "text-primary mr-3" ]
                [ fas "fa-arrow-up" [] ]

        moveDownButton =
            a [ onClick (MoveComponentDown uuid), class "text-primary mr-3" ]
                [ fas "fa-arrow-down" [] ]

        deleteButton =
            a [ onClick (DeleteComponent uuid), class "text-danger" ]
                [ fas "fa-trash" [] ]

        controlButtons =
            if controls then
                div []
                    [ moveUpButton
                    , moveDownButton
                    , deleteButton
                    ]

            else
                emptyNode
    in
    div [ class "card" ]
        [ div [ class "card-header d-flex justify-content-between" ]
            [ div []
                [ fa icon [ class "mr-1" ]
                , text label
                ]
            , controlButtons
            ]
        , div [ class "card-body" ] content
        ]
