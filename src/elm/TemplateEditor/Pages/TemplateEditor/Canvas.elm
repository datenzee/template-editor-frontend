module TemplateEditor.Pages.TemplateEditor.Canvas exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, form, h5, hr, i, input, label, pre, text, textarea)
import Html.Attributes exposing (checked, class, classList, readonly, rows, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Extra exposing (emptyNode)
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Set exposing (Set)
import TemplateEditor.Common.FontAwesome exposing (fa, far, fas)
import TemplateEditor.Common.Setters exposing (setComponentType, setContent, setPredicate, setValue)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.DUIO.App as App exposing (App)
import TemplateEditor.Data.DUIO.Component as Component exposing (Component(..), Condition, Container, ContentComponent, ContentComponentContent(..), ContentComponentType(..), IterativeContainer)
import TemplateEditor.Ports as Ports
import Uuid exposing (Uuid)


type alias Model =
    { app : App
    , dropdownStates : Dict String Dropdown.State
    , collapsedComponents : Set String
    , moveModalUuid : Maybe Uuid
    , moveModalValue : String
    , moveModalError : Maybe String
    }


init : App -> Model
init app =
    { app = app
    , dropdownStates = Dict.empty
    , collapsedComponents = Set.empty
    , moveModalUuid = Nothing
    , moveModalValue = ""
    , moveModalError = Nothing
    }


type Msg
    = DropdownMsg String Dropdown.State
    | AddContainer Uuid
    | AddIterativeContainer Uuid
    | AddCondition Uuid
    | AddContentComponent Uuid
    | UpdateIsBlock Uuid Bool
    | UpdateIterativeContainerPredicate Uuid String
    | UpdateConditionPredicate Uuid String
    | UpdateConditionValue Uuid String
    | UpdateContentComponentType Uuid ContentComponentType
    | UpdateContentComponentContent Uuid ContentComponentContent
    | MoveComponentUp Uuid
    | MoveComponentDown Uuid
    | DeleteComponent Uuid
    | CollapseComponent Uuid
    | ExpandComponent Uuid
    | CollapseAll
    | ExpandAll
    | CopyUuid Uuid
    | MoveModalSetUuid (Maybe Uuid)
    | MoveModalSetValue String
    | MoveModalConfirm


update : AppState -> Msg -> Model -> ( Seed, Model, Cmd Msg )
update appState msg model =
    let
        wrap m =
            ( appState.seed, m, Cmd.none )

        updateRootComponent rootComponent =
            let
                app =
                    model.app
            in
            { model | app = { app | rootComponent = rootComponent } }

        addComponent parentUuid component currentModel =
            let
                insert : Container -> Container
                insert parentContainer =
                    { parentContainer | contains = parentContainer.contains ++ [ component ] }
            in
            updateRootComponent <|
                mapContainer (mapHelper parentUuid insert) currentModel.app.rootComponent

        deleteComponent uuid currentModel =
            let
                delete : Container -> Container
                delete container =
                    { container | contains = List.filter ((/=) uuid << Component.getUuid) container.contains }

                rootComponent =
                    mapContainer delete currentModel.app.rootComponent
            in
            updateRootComponent rootComponent

        updateComponent uuid mapFn updateFn =
            updateRootComponent <|
                mapFn (mapHelper uuid updateFn) model.app.rootComponent
    in
    case msg of
        DropdownMsg id state ->
            wrap { model | dropdownStates = Dict.insert id state model.dropdownStates }

        AddContainer parentUuid ->
            let
                ( uuid, newSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                container =
                    ContainerComponent
                        { uuid = uuid
                        , contains = []
                        , isBlock = False
                        }
            in
            ( newSeed, addComponent parentUuid container model, Cmd.none )

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
                            , isBlock = False
                            }
                        , isBlock = False
                        }
            in
            ( newSeed2, addComponent parentUuid iterativeContainer model, Cmd.none )

        AddCondition parentUuid ->
            let
                ( uuid1, newSeed1 ) =
                    Random.step Uuid.uuidGenerator appState.seed

                ( uuid2, newSeed2 ) =
                    Random.step Uuid.uuidGenerator newSeed1

                ( uuid3, newSeed3 ) =
                    Random.step Uuid.uuidGenerator newSeed2

                condition =
                    ConditionComponent
                        { uuid = uuid1
                        , predicate = ""
                        , value = ""
                        , positiveContent =
                            { uuid = uuid2
                            , contains = []
                            , isBlock = False
                            }
                        , negativeContent =
                            { uuid = uuid3
                            , contains = []
                            , isBlock = False
                            }
                        , isBlock = False
                        }
            in
            ( newSeed3, addComponent parentUuid condition model, Cmd.none )

        AddContentComponent parentUuid ->
            let
                ( uuid, newSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                contentComponent =
                    ContentComponentComponent
                        { uuid = uuid
                        , componentType = HeadingContentComponentType
                        , content = ContentComponentPredicate ""
                        , isBlock = False
                        }
            in
            ( newSeed, addComponent parentUuid contentComponent model, Cmd.none )

        UpdateIsBlock uuid isBlock ->
            let
                app =
                    model.app
            in
            wrap { model | app = { app | rootComponent = updateIsBlock isBlock uuid app.rootComponent } }

        UpdateIterativeContainerPredicate uuid predicate ->
            wrap <| updateComponent uuid mapIterativeContainer (setPredicate predicate)

        UpdateConditionPredicate uuid predicate ->
            wrap <| updateComponent uuid mapCondition (setPredicate predicate)

        UpdateConditionValue uuid predicate ->
            wrap <| updateComponent uuid mapCondition (setValue predicate)

        UpdateContentComponentType uuid type_ ->
            wrap <| updateComponent uuid mapContentComponent (setComponentType type_)

        UpdateContentComponentContent uuid content ->
            wrap <| updateComponent uuid mapContentComponent (setContent content)

        MoveComponentUp uuid ->
            let
                move : Container -> Container
                move container =
                    { container | contains = moveUp uuid container.contains }

                rootComponent =
                    mapContainer move model.app.rootComponent
            in
            wrap <| updateRootComponent rootComponent

        MoveComponentDown uuid ->
            let
                move : Container -> Container
                move container =
                    { container | contains = moveDown uuid container.contains }

                rootComponent =
                    mapContainer move model.app.rootComponent
            in
            wrap <| updateRootComponent rootComponent

        DeleteComponent uuid ->
            wrap <| deleteComponent uuid model

        CollapseComponent uuid ->
            wrap { model | collapsedComponents = Set.insert (Uuid.toString uuid) model.collapsedComponents }

        ExpandComponent uuid ->
            wrap { model | collapsedComponents = Set.remove (Uuid.toString uuid) model.collapsedComponents }

        CollapseAll ->
            let
                getUuids component =
                    case component of
                        ContainerComponent container ->
                            container.uuid :: List.concatMap getUuids container.contains

                        IterativeContainerComponent iterativeContainer ->
                            iterativeContainer.uuid :: getUuids (ContainerComponent iterativeContainer.content)

                        ConditionComponent condition ->
                            condition.uuid :: getUuids (ContainerComponent condition.positiveContent) ++ getUuids (ContainerComponent condition.negativeContent)

                        ContentComponentComponent { uuid } ->
                            [ uuid ]
            in
            wrap { model | collapsedComponents = Set.fromList <| List.map Uuid.toString <| getUuids model.app.rootComponent }

        ExpandAll ->
            wrap { model | collapsedComponents = Set.empty }

        CopyUuid uuid ->
            ( appState.seed, model, Ports.copyToClipboard (Uuid.toString uuid) )

        MoveModalSetUuid mbUuid ->
            wrap { model | moveModalUuid = mbUuid, moveModalValue = "", moveModalError = Nothing }

        MoveModalSetValue value ->
            wrap { model | moveModalValue = value }

        MoveModalConfirm ->
            case model.moveModalUuid of
                Just moveModalUuid ->
                    let
                        mbMovingComponent =
                            getComponent moveModalUuid model.app.rootComponent

                        mbParentComponent =
                            getComponent (Uuid.fromUuidString model.moveModalValue) model.app.rootComponent
                    in
                    case ( mbMovingComponent, mbParentComponent ) of
                        ( Just movingComponent, Just parentComponent ) ->
                            let
                                newModel =
                                    model
                                        |> deleteComponent (Component.getUuid movingComponent)
                                        |> addComponent (Component.getUuid parentComponent) movingComponent
                            in
                            wrap { newModel | moveModalUuid = Nothing }

                        _ ->
                            wrap { model | moveModalError = Just "Invalid component UUID." }

                Nothing ->
                    wrap model


updateIsBlock : Bool -> Uuid -> Component -> Component
updateIsBlock isBlock uuid component =
    case component of
        ContainerComponent container ->
            if container.uuid == uuid then
                ContainerComponent { container | isBlock = isBlock }

            else
                ContainerComponent { container | contains = List.map (updateIsBlock isBlock uuid) container.contains }

        IterativeContainerComponent iterativeContainer ->
            if iterativeContainer.uuid == uuid then
                IterativeContainerComponent { iterativeContainer | isBlock = isBlock }

            else
                let
                    container =
                        iterativeContainer.content
                in
                if container.uuid == uuid then
                    IterativeContainerComponent { iterativeContainer | content = { container | isBlock = isBlock } }

                else
                    IterativeContainerComponent { iterativeContainer | content = { container | contains = List.map (updateIsBlock isBlock uuid) container.contains } }

        ConditionComponent condition ->
            if condition.uuid == uuid then
                ConditionComponent { condition | isBlock = isBlock }

            else
                let
                    positiveContent =
                        condition.positiveContent

                    negativeContent =
                        condition.negativeContent
                in
                if positiveContent.uuid == uuid then
                    ConditionComponent { condition | positiveContent = { positiveContent | isBlock = isBlock } }

                else if negativeContent.uuid == uuid then
                    ConditionComponent { condition | negativeContent = { negativeContent | isBlock = isBlock } }

                else
                    ConditionComponent
                        { condition
                            | positiveContent = { positiveContent | contains = List.map (updateIsBlock isBlock uuid) positiveContent.contains }
                            , negativeContent = { negativeContent | contains = List.map (updateIsBlock isBlock uuid) negativeContent.contains }
                        }

        ContentComponentComponent contentComponent ->
            if contentComponent.uuid == uuid then
                ContentComponentComponent { contentComponent | isBlock = isBlock }

            else
                ContentComponentComponent contentComponent


mapContainer : (Container -> Container) -> Component -> Component
mapContainer map component =
    let
        mapContainer_ container =
            map { container | contains = List.map (mapContainer map) container.contains }
    in
    case component of
        ContainerComponent container ->
            ContainerComponent <|
                mapContainer_ container

        IterativeContainerComponent iterativeContainer ->
            IterativeContainerComponent { iterativeContainer | content = mapContainer_ iterativeContainer.content }

        ConditionComponent condition ->
            ConditionComponent
                { condition
                    | positiveContent = mapContainer_ condition.positiveContent
                    , negativeContent = mapContainer_ condition.negativeContent
                }

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

        ConditionComponent condition ->
            ConditionComponent
                { condition
                    | positiveContent = mapContainer_ condition.positiveContent
                    , negativeContent = mapContainer_ condition.negativeContent
                }

        other ->
            other


mapCondition : (Condition -> Condition) -> Component -> Component
mapCondition map component =
    let
        mapContainer_ container =
            { container | contains = List.map (mapCondition map) container.contains }
    in
    case component of
        ContainerComponent container ->
            ContainerComponent <| mapContainer_ container

        IterativeContainerComponent iterativeContainer ->
            IterativeContainerComponent <|
                { iterativeContainer | content = mapContainer_ iterativeContainer.content }

        ConditionComponent condition ->
            ConditionComponent <|
                map
                    { condition
                        | positiveContent = mapContainer_ condition.positiveContent
                        , negativeContent = mapContainer_ condition.negativeContent
                    }

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

        ConditionComponent condition ->
            ConditionComponent
                { condition
                    | positiveContent = mapContainer_ condition.positiveContent
                    , negativeContent = mapContainer_ condition.negativeContent
                }

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


getComponent : Uuid -> Component -> Maybe Component
getComponent uuid component =
    if Component.getUuid component == uuid then
        Just component

    else
        let
            fold : Component -> Maybe Component -> Maybe Component
            fold comp acc =
                if Maybe.isJust acc then
                    acc

                else
                    getComponent uuid comp

            foldChildren =
                List.foldl fold Nothing
        in
        case component of
            ContainerComponent container ->
                foldChildren container.contains

            IterativeContainerComponent iterativeContainer ->
                if iterativeContainer.content.uuid == uuid then
                    Just (ContainerComponent iterativeContainer.content)

                else
                    foldChildren iterativeContainer.content.contains

            ConditionComponent condition ->
                if condition.positiveContent.uuid == uuid then
                    Just (ContainerComponent condition.positiveContent)

                else if condition.negativeContent.uuid == uuid then
                    Just (ContainerComponent condition.negativeContent)

                else
                    foldChildren condition.positiveContent.contains
                        |> Maybe.orElse (foldChildren condition.negativeContent.contains)

            ContentComponentComponent _ ->
                Nothing


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
    div [ class "app-canvas" ]
        [ div [ class "mb-3" ]
            [ a [ onClick ExpandAll, class "text-primary me-2" ] [ text "Expand all" ]
            , a [ onClick CollapseAll, class "text-primary" ] [ text "Collapse all" ]
            ]
        , viewApp model
        , viewMoveModal model
        ]


viewApp : Model -> Html Msg
viewApp model =
    let
        rootContainer =
            case model.app.rootComponent of
                ContainerComponent container ->
                    viewContainer model False container

                _ ->
                    viewComponent model model.app.rootComponent
    in
    --viewCard { icon = "far fa-window-maximize", label = "App", uuid = Uuid.nil, controls = False }
    --    model
    rootContainer


viewComponent : Model -> Component -> Html Msg
viewComponent model component =
    case component of
        ContainerComponent container ->
            viewContainer model True container

        IterativeContainerComponent iterativeContainer ->
            viewCard { icon = "fas fa-sync-alt", label = "IterativeContainer", uuid = iterativeContainer.uuid, controls = True, copyUuid = False, isBlock = iterativeContainer.isBlock }
                model
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
                , viewContainer model False iterativeContainer.content
                ]

        ConditionComponent condition ->
            viewCard { icon = "fas fa-code-branch", label = "Condition", uuid = condition.uuid, controls = True, copyUuid = False, isBlock = condition.isBlock }
                model
                [ div [ class "form-group row mb-2" ]
                    [ label [ class "col-md-2 col-form-label" ] [ text "Predicate" ]
                    , div [ class "col-md-10" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , onInput (UpdateConditionPredicate condition.uuid)
                            , value condition.predicate
                            ]
                            []
                        ]
                    ]
                , div [ class "form-group row" ]
                    [ label [ class "col-md-2 col-form-label" ] [ text "Value" ]
                    , div [ class "col-md-10" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , onInput (UpdateConditionValue condition.uuid)
                            , value condition.value
                            ]
                            []
                        ]
                    ]
                , hr [] []
                , viewContainerWithLabel "Positive Content" model False condition.positiveContent
                , viewContainerWithLabel "Negative Content" model False condition.negativeContent
                ]

        ContentComponentComponent contentComponent ->
            let
                getComponentIconAndName componentType =
                    case componentType of
                        HeadingContentComponentType ->
                            ( "fas fa-heading", "Heading" )

                        TextContentComponentType ->
                            ( "fas fa-paragraph", "Text" )

                        StrongContentComponentType ->
                            ( "fas fa-bold", "Strong" )

                        EmphasisContentComponentType ->
                            ( "fas fa-italic", "Emphasis" )

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
                        [ fa icon [ class "me-2 text-muted fa-fw" ], text label ]

                componentTitleDropdown =
                    Dropdown.dropdown typeDropdownState
                        { options = [ Dropdown.attrs [ class "dropdown-text" ] ]
                        , toggleMsg = DropdownMsg typeDropdownId
                        , toggleButton =
                            Dropdown.toggle [ Button.roleLink ]
                                [ fa componentIcon [ class "me-1 fa-fw" ]
                                , text componentName
                                ]
                        , items =
                            List.map typeDropdownItem
                                [ HeadingContentComponentType
                                , TextContentComponentType
                                , StrongContentComponentType
                                , EmphasisContentComponentType
                                ]
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
            viewCardExtra { component = componentTitleDropdown, uuid = contentComponent.uuid, controls = True, copyUuid = False, isBlock = contentComponent.isBlock }
                model
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
viewContainer =
    viewContainerWithLabel "Container"


viewContainerWithLabel : String -> Model -> Bool -> Container -> Html Msg
viewContainerWithLabel containerLabel model controls container =
    let
        dropdownId =
            "container-" ++ Uuid.toString container.uuid

        children =
            List.map (viewComponent model) container.contains

        dropdownState =
            Maybe.withDefault Dropdown.initialState <|
                Dict.get dropdownId model.dropdownStates

        dropdownItem icon label msg =
            Dropdown.buttonItem [ onClick msg ] [ fa icon [ class "me-2 text-muted" ], text label ]

        dropdown =
            Dropdown.dropdown dropdownState
                { options = []
                , toggleMsg = DropdownMsg dropdownId
                , toggleButton =
                    Dropdown.toggle [ Button.outlineSecondary, Button.small ]
                        [ fas "fa-plus" [ class "me-1" ]
                        , text "Add"
                        ]
                , items =
                    [ dropdownItem "fas fa-th-large" "Container" (AddContainer container.uuid)
                    , dropdownItem "fas fa-sync-alt" "IterativeContainer" (AddIterativeContainer container.uuid)
                    , dropdownItem "fas fa-code-branch" "Condition" (AddCondition container.uuid)
                    , dropdownItem "far fa-file-alt" "Content" (AddContentComponent container.uuid)
                    ]
                }
    in
    viewCard { icon = "fas fa-th-large", label = containerLabel, uuid = container.uuid, controls = controls, copyUuid = True, isBlock = container.isBlock }
        model
        (children ++ [ dropdown ])


type alias ViewCardConfig =
    { icon : String
    , label : String
    , uuid : Uuid
    , controls : Bool
    , copyUuid : Bool
    , isBlock : Bool
    }


viewCard : ViewCardConfig -> Model -> List (Html Msg) -> Html Msg
viewCard { icon, label, uuid, controls, copyUuid, isBlock } =
    viewCardExtra
        { component =
            div []
                [ fa icon [ class "me-1 fa-fw" ]
                , text label
                ]
        , uuid = uuid
        , controls = controls
        , copyUuid = copyUuid
        , isBlock = isBlock
        }


type alias ViewCardExtraConfig =
    { component : Html Msg
    , uuid : Uuid
    , controls : Bool
    , copyUuid : Bool
    , isBlock : Bool
    }


viewCardExtra : ViewCardExtraConfig -> Model -> List (Html Msg) -> Html Msg
viewCardExtra { component, uuid, controls, copyUuid, isBlock } model content =
    let
        isCollapsed =
            Set.member (Uuid.toString uuid) model.collapsedComponents

        moveButton =
            a [ onClick (MoveModalSetUuid (Just uuid)), class "text-primary ms-3" ]
                [ fas "fa-reply" [] ]

        moveUpButton =
            a [ onClick (MoveComponentUp uuid), class "text-primary ms-3" ]
                [ fas "fa-arrow-up" [] ]

        moveDownButton =
            a [ onClick (MoveComponentDown uuid), class "text-primary ms-3" ]
                [ fas "fa-arrow-down" [] ]

        deleteButton =
            a [ onClick (DeleteComponent uuid), class "text-danger ms-3" ]
                [ fas "fa-trash" [] ]

        collapseButton =
            if isCollapsed then
                a [ onClick (ExpandComponent uuid), class "text-primary me-3" ] [ fas "fa-chevron-right fa-fw" [] ]

            else
                a [ onClick (CollapseComponent uuid), class "text-primary me-3" ] [ fas "fa-chevron-down fa-fw" [] ]

        copyUuidButton =
            if copyUuid then
                a [ class "text-muted small", onClick (CopyUuid uuid) ]
                    [ far "fa-copy me-1" []
                    , text (Maybe.withDefault "uuid" (List.head (String.split "-" (Uuid.toString uuid))))
                    ]

            else
                emptyNode

        controlButtons =
            if controls then
                div []
                    [ copyUuidButton
                    , moveButton
                    , moveUpButton
                    , moveDownButton
                    , deleteButton
                    ]

            else
                div [] [ copyUuidButton ]

        blockCheckbox =
            label [ class "ms-3 cursor-pointer" ]
                [ input [ type_ "checkbox", checked isBlock, onCheck (UpdateIsBlock uuid), class "me-1" ] []
                , text "Block"
                ]

        body =
            if Set.member (Uuid.toString uuid) model.collapsedComponents then
                emptyNode

            else
                div [ class "card-body" ] content
    in
    div [ class "card" ]
        [ div [ class "card-header d-flex justify-content-between" ]
            [ div [ class "d-flex align-items-end" ] [ collapseButton, component, blockCheckbox ]
            , controlButtons
            ]
        , body
        ]


viewMoveModal : Model -> Html Msg
viewMoveModal model =
    let
        errorBlock =
            case model.moveModalError of
                Just error ->
                    div [ class "text-danger mb-3" ] [ text error ]

                Nothing ->
                    emptyNode
    in
    div [ class "modal fade", classList [ ( "show", Maybe.isJust model.moveModalUuid ) ] ]
        [ div [ class "modal-dialog modal-dialog-centered" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text "Move" ] ]
                , div [ class "modal-body" ]
                    [ errorBlock
                    , label [] [ text "New parent UUID" ]
                    , div []
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , onInput MoveModalSetValue
                            , value model.moveModalValue
                            ]
                            []
                        ]
                    ]
                , div [ class "modal-footer justify-content-between" ]
                    [ button [ class "btn btn-secondary", onClick (MoveModalSetUuid Nothing) ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick MoveModalConfirm ] [ text "Move" ]
                    ]
                ]
            ]
        ]
