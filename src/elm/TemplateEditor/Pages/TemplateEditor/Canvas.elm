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
import TemplateEditor.Common.Setters exposing (setComponentType, setContent, setPredicate)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.DUIO.App as App exposing (App)
import TemplateEditor.Data.DUIO.Component as Component exposing (Component(..), Container, ContentComponent, ContentComponentContent(..), ContentComponentType(..), IterativeContainer)
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
    | AddContentComponent Uuid
    | UpdateIterativeContainerPredicate Uuid String
    | UpdateContentComponentType Uuid ContentComponentType
    | UpdateContentComponentContent Uuid ContentComponentContent
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
        DropdownMsg id state ->
            ( appState.seed
            , { model | dropdownStates = Dict.insert id state model.dropdownStates }
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

        AddContentComponent parentUuid ->
            let
                ( uuid, newSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                contentComponent =
                    ContentComponentComponent
                        { uuid = uuid
                        , componentType = HeadingContentComponentType
                        , content = ContentComponentText ""
                        }
            in
            ( newSeed, addComponent parentUuid contentComponent )

        UpdateIterativeContainerPredicate uuid predicate ->
            ( appState.seed
            , updateComponent uuid mapIterativeContainer (setPredicate predicate)
            )

        UpdateContentComponentType uuid type_ ->
            ( appState.seed
            , updateComponent uuid mapContentComponent (setComponentType type_)
            )

        UpdateContentComponentContent uuid content ->
            ( appState.seed
            , updateComponent uuid mapContentComponent (setContent content)
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


mapContentComponent : (ContentComponent -> ContentComponent) -> Component -> Component
mapContentComponent map component =
    let
        mapContainer_ container =
            { container | contains = List.map (mapContentComponent map) container.contains }
    in
    case component of
        ContainerComponent container ->
            ContainerComponent <| mapContainer_ container

        IterativeContainerComponent iterativeContainer ->
            IterativeContainerComponent <|
                { iterativeContainer | content = mapContainer_ iterativeContainer.content }

        ContentComponentComponent contentComponent ->
            ContentComponentComponent <|
                map contentComponent


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
        toDropdownSub ( id, state ) =
            Dropdown.subscriptions state (DropdownMsg id)
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

        ContentComponentComponent contentComponent ->
            let
                getComponentIconAndName componentType =
                    case componentType of
                        HeadingContentComponentType ->
                            ( "fas fa-heading", "Heading" )

                        ParagraphContentComponentType ->
                            ( "fas fa-paragraph", "Paragraph" )

                ( componentIcon, componentName ) =
                    getComponentIconAndName contentComponent.componentType

                typeDropdownId =
                    "content-type-" ++ Uuid.toString contentComponent.uuid

                typeDropdownState =
                    Maybe.withDefault Dropdown.initialState <|
                        Dict.get typeDropdownId model.dropdownStates

                typeDropdownItem componentType =
                    let
                        ( icon, label ) =
                            getComponentIconAndName componentType
                    in
                    Dropdown.buttonItem [ onClick (UpdateContentComponentType contentComponent.uuid componentType) ]
                        [ fa icon [ class "mr-2 text-muted" ], text label ]

                componentTitleDropdown =
                    Dropdown.dropdown typeDropdownState
                        { options = [ Dropdown.attrs [ class "dropdown-text" ] ]
                        , toggleMsg = DropdownMsg typeDropdownId
                        , toggleButton =
                            Dropdown.toggle [ Button.roleLink ]
                                [ fa componentIcon [ class "mr-1" ]
                                , text componentName
                                ]
                        , items = List.map typeDropdownItem [ HeadingContentComponentType, ParagraphContentComponentType ]
                        }

                ( contentValue, toContent ) =
                    case contentComponent.content of
                        ContentComponentPredicate value ->
                            ( value, ContentComponentPredicate )

                        ContentComponentText value ->
                            ( value, ContentComponentText )

                getContentLabel componentContent =
                    case componentContent of
                        ContentComponentPredicate _ ->
                            "Predicate"

                        ContentComponentText _ ->
                            "Text"

                contentDropdownId =
                    "content-content-" ++ Uuid.toString contentComponent.uuid

                contentDropdownState =
                    Maybe.withDefault Dropdown.initialState <|
                        Dict.get contentDropdownId model.dropdownStates

                contentDropdownItem content =
                    let
                        newContent =
                            content contentValue

                        label =
                            getContentLabel newContent
                    in
                    Dropdown.buttonItem
                        [ onClick (UpdateContentComponentContent contentComponent.uuid newContent) ]
                        [ text label ]

                componentContentLabelDropdown =
                    Dropdown.dropdown contentDropdownState
                        { options = [ Dropdown.attrs [ class "dropdown-text" ] ]
                        , toggleMsg = DropdownMsg contentDropdownId
                        , toggleButton =
                            Dropdown.toggle [ Button.roleLink ]
                                [ text (getContentLabel contentComponent.content) ]
                        , items = List.map contentDropdownItem [ ContentComponentPredicate, ContentComponentText ]
                        }
            in
            viewCardExtra { component = componentTitleDropdown, uuid = contentComponent.uuid, controls = True }
                [ div [ class "form-group row" ]
                    [ label
                        [ class "col-md-2 col-form-label"
                        ]
                        [ componentContentLabelDropdown ]
                    , div [ class "col-md-10" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , onInput (UpdateContentComponentContent contentComponent.uuid << toContent)
                            , value contentValue
                            ]
                            []
                        ]
                    ]
                ]


viewContainer : Model -> Bool -> Container -> Html Msg
viewContainer model contentOnly container =
    let
        dropdownId =
            "container-" ++ Uuid.toString container.uuid

        children =
            List.map (viewComponent model) container.contains

        dropdownState =
            Maybe.withDefault Dropdown.initialState <|
                Dict.get dropdownId model.dropdownStates

        dropdownItem icon label msg =
            Dropdown.buttonItem [ onClick msg ] [ fa icon [ class "mr-2 text-muted" ], text label ]

        dropdown =
            Dropdown.dropdown dropdownState
                { options = []
                , toggleMsg = DropdownMsg dropdownId
                , toggleButton =
                    Dropdown.toggle [ Button.outlineSecondary, Button.small ]
                        [ fas "fa-plus" [ class "mr-1" ]
                        , text "Add"
                        ]
                , items =
                    [ dropdownItem "far fa-square" "Container" (AddContainer container.uuid)
                    , dropdownItem "fas fa-sync-alt" "IterativeContainer" (AddIterativeContainer container.uuid)
                    , dropdownItem "far fa-file-alt" "Content" (AddContentComponent container.uuid)
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
viewCard { icon, label, uuid, controls } =
    viewCardExtra
        { component =
            div []
                [ fa icon [ class "mr-1" ]
                , text label
                ]
        , uuid = uuid
        , controls = controls
        }


type alias ViewCardExtraConfig =
    { component : Html Msg
    , uuid : Uuid
    , controls : Bool
    }


viewCardExtra : ViewCardExtraConfig -> List (Html Msg) -> Html Msg
viewCardExtra { component, uuid, controls } content =
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
            [ component
            , controlButtons
            ]
        , div [ class "card-body" ] content
        ]
