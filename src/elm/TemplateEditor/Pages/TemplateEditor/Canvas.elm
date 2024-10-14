module TemplateEditor.Pages.TemplateEditor.Canvas exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h5, hr, input, label, li, option, select, small, span, strong, text, ul)
import Html.Attributes exposing (checked, class, classList, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Extra exposing (emptyNode)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Set exposing (Set)
import TemplateEditor.Common.FontAwesome exposing (fa, fas)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.ViewOntology.App as App exposing (App)
import TemplateEditor.Data.ViewOntology.Component exposing (Condition, Content, ContentContent(..), DataComponent, DataComponentWrapper, IterativeContainerData, Leaf, LeafContent(..), LeafContentSource(..), Node, NodeContent(..), Tree, TreeContent(..), contentContentFromString, contentContentToString, initLeafContentContent, initLeafContentDataComponentWrapper, initNodeContentCondition, initNodeContentIterativeContainer, initTreeContainer, initTreeContent, sortTrees)
import Uuid exposing (Uuid)


type alias Model =
    { app : App
    , addComponentModalOpen : Bool
    , addComponentModalValue : String
    , addComponentModalError : Maybe String
    , openedDataComponent : Maybe DataComponent
    , dataComponentToDelete : Maybe DataComponent
    , dropdownStates : Dict String Dropdown.State
    , collapsedComponents : Set String
    }


init : App -> Model
init app =
    { app = app
    , addComponentModalOpen = False
    , addComponentModalValue = ""
    , addComponentModalError = Nothing
    , openedDataComponent = List.head app.components
    , dataComponentToDelete = Nothing
    , dropdownStates = Dict.empty
    , collapsedComponents = Set.empty
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
    | ExpandComponent Uuid
    | CollapseComponent Uuid
    | UpdateNodeType Uuid NodeType
    | UpdateLeafType Uuid LeafType
    | UpdateIsBlock Uuid Bool
    | AddNode Uuid
    | AddLeaf Uuid
    | DeleteTree Uuid
    | DropdownMsg String Dropdown.State
    | UpdateConditionPredicate Uuid String
    | UpdateConditionValue Uuid String
    | UpdateIterativeContainerPredicate Uuid String
    | UpdateDataComponentPredicate Uuid String
    | UpdateDataComponentDataComponent Uuid String
    | UpdateLeafContentSource Uuid LeafContentSource
    | UpdateLeafContentContent Uuid String


type NodeType
    = NodeTypeStrong
    | NodeTypeEmphasis
    | NodeTypeHeading
    | NodeTypeCondition
    | NodeTypeContainer
    | NodeTypeIterativeContainer


type LeafType
    = LeafTypeDataComponentWrapper
    | LeafTypeContent


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
                    ( newApp, newSeed ) =
                        App.addDataComponent appState.seed model.addComponentModalValue model.app
                in
                ( newSeed
                , { model
                    | addComponentModalOpen = False
                    , app = newApp
                    , openedDataComponent = List.last newApp.components
                  }
                , Cmd.none
                )

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

        CollapseComponent uuid ->
            wrap { model | collapsedComponents = Set.insert (Uuid.toString uuid) model.collapsedComponents }

        ExpandComponent uuid ->
            wrap { model | collapsedComponents = Set.remove (Uuid.toString uuid) model.collapsedComponents }

        UpdateNodeType uuid nodeType ->
            let
                ( newNodeContent, newSeed ) =
                    case nodeType of
                        NodeTypeStrong ->
                            ( NodeContentStrong, appState.seed )

                        NodeTypeEmphasis ->
                            ( NodeContentEmphasis, appState.seed )

                        NodeTypeHeading ->
                            ( NodeContentHeading, appState.seed )

                        NodeTypeCondition ->
                            initNodeContentCondition appState.seed

                        NodeTypeContainer ->
                            ( NodeContentContainer, appState.seed )

                        NodeTypeIterativeContainer ->
                            ( initNodeContentIterativeContainer, appState.seed )

                updateNodeType node =
                    { node | content = newNodeContent }

                updateComponent component =
                    { component | content = updateNode updateNodeType uuid component.content }
            in
            ( newSeed, updateCurrentComponent updateComponent model, Cmd.none )

        UpdateLeafType uuid leafType ->
            let
                newLeafContent =
                    case leafType of
                        LeafTypeContent ->
                            initLeafContentContent

                        LeafTypeDataComponentWrapper ->
                            initLeafContentDataComponentWrapper

                updateLeafType leaf =
                    { leaf | content = newLeafContent }

                updateComponent component =
                    { component | content = updateLeaf updateLeafType uuid component.content }
            in
            ( appState.seed, updateCurrentComponent updateComponent model, Cmd.none )

        UpdateIsBlock uuid isBlock ->
            let
                updateIsBlock node =
                    { node | isBlock = isBlock }

                updateComponent component =
                    { component | content = updateNode updateIsBlock uuid component.content }
            in
            ( appState.seed, updateCurrentComponent updateComponent model, Cmd.none )

        AddNode parentUuid ->
            let
                ( tree, newSeed ) =
                    initTreeContainer appState.seed

                updateParent node =
                    { node | contains = node.contains ++ [ tree ] }

                updateComponent component =
                    { component | content = updateNode updateParent parentUuid component.content }
            in
            ( newSeed, updateCurrentComponent updateComponent model, Cmd.none )

        AddLeaf parentUuid ->
            let
                ( tree, newSeed ) =
                    initTreeContent appState.seed

                updateParent node =
                    { node | contains = node.contains ++ [ tree ] }

                updateComponent component =
                    { component | content = updateNode updateParent parentUuid component.content }
            in
            ( newSeed, updateCurrentComponent updateComponent model, Cmd.none )

        DeleteTree uuid ->
            let
                updateParent _ node =
                    { node | contains = List.filter ((/=) uuid << .uuid) node.contains }

                updateComponent component =
                    { component | content = mapNodes updateParent component.content }
            in
            ( appState.seed, updateCurrentComponent updateComponent model, Cmd.none )

        DropdownMsg id state ->
            wrap { model | dropdownStates = Dict.insert id state model.dropdownStates }

        UpdateConditionPredicate uuid predicate ->
            wrap <| updateCondition uuid (\condition -> { condition | predicate = predicate }) model

        UpdateConditionValue uuid value ->
            wrap <| updateCondition uuid (\condition -> { condition | value = value }) model

        UpdateIterativeContainerPredicate uuid predicate ->
            wrap <| updateIterativeContainer uuid (\ic -> { ic | predicate = predicate }) model

        UpdateDataComponentPredicate uuid predicate ->
            wrap <| updateDataComponent uuid (\dcw -> { dcw | predicate = predicate }) model

        UpdateDataComponentDataComponent uuid dataComponent ->
            wrap <| updateDataComponent uuid (\dcw -> { dcw | dataComponent = dataComponent }) model

        UpdateLeafContentSource uuid contentSource ->
            wrap <| updateContent uuid (\content -> { content | contentSource = contentSource }) model

        UpdateLeafContentContent uuid contentContent ->
            wrap <| updateContent uuid (\content -> { content | content = contentContentFromString contentContent }) model

        _ ->
            ( appState.seed, model, Cmd.none )


updateContent : Uuid -> (Content -> Content) -> Model -> Model
updateContent uuid updateFn model =
    let
        updateLeafContentSource leaf =
            case leaf.content of
                LeafContentContent content ->
                    { leaf | content = LeafContentContent (updateFn content) }

                _ ->
                    leaf

        updateComponent component =
            { component | content = updateLeaf updateLeafContentSource uuid component.content }
    in
    updateCurrentComponent updateComponent model


updateDataComponent : Uuid -> (DataComponentWrapper -> DataComponentWrapper) -> Model -> Model
updateDataComponent uuid updateFn model =
    let
        updateDataComponentLeaf leaf =
            case leaf.content of
                LeafContentDataComponentWrapper dcw ->
                    { leaf | content = LeafContentDataComponentWrapper (updateFn dcw) }

                _ ->
                    leaf

        updateComponent component =
            { component | content = updateLeaf updateDataComponentLeaf uuid component.content }
    in
    updateCurrentComponent updateComponent model


updateCondition : Uuid -> (Condition -> Condition) -> Model -> Model
updateCondition uuid updateFn model =
    let
        updateConditionNode node =
            case node.content of
                NodeContentCondition condition ->
                    { node | content = NodeContentCondition (updateFn condition) }

                _ ->
                    node

        updateComponent component =
            { component | content = updateNode updateConditionNode uuid component.content }
    in
    updateCurrentComponent updateComponent model


updateIterativeContainer : Uuid -> (IterativeContainerData -> IterativeContainerData) -> Model -> Model
updateIterativeContainer uuid updateFn model =
    let
        updateIterativeContainerNode node =
            case node.content of
                NodeContentIterativeContainer iterativeContainer ->
                    { node | content = NodeContentIterativeContainer (updateFn iterativeContainer) }

                _ ->
                    node

        updateComponent component =
            { component | content = updateNode updateIterativeContainerNode uuid component.content }
    in
    updateCurrentComponent updateComponent model


updateCurrentComponent : (DataComponent -> DataComponent) -> Model -> Model
updateCurrentComponent updateFn model =
    case model.openedDataComponent of
        Just dataComponent ->
            let
                newComponent =
                    updateFn dataComponent
            in
            { model
                | app = App.updateDataComponent dataComponent.name newComponent model.app
                , openedDataComponent = Just newComponent
            }

        Nothing ->
            model


updateNode : (Node -> Node) -> Uuid -> Tree -> Tree
updateNode updateFn targetUuid =
    let
        updateFn_ treeUuid node =
            if treeUuid == targetUuid then
                updateFn node

            else
                node
    in
    mapNodes updateFn_


mapNodes : (Uuid -> Node -> Node) -> Tree -> Tree
mapNodes mapNode tree =
    case tree.content of
        TreeContentNode node ->
            case node.content of
                NodeContentCondition condition ->
                    { tree
                        | content =
                            TreeContentNode
                                (mapNode tree.uuid
                                    { node
                                        | content =
                                            NodeContentCondition
                                                { condition
                                                    | containsPositive = mapNodes mapNode condition.containsPositive
                                                    , containsNegative = mapNodes mapNode condition.containsNegative
                                                }
                                    }
                                )
                    }

                _ ->
                    { tree
                        | content =
                            TreeContentNode
                                (mapNode tree.uuid { node | contains = List.map (mapNodes mapNode) node.contains })
                    }

        _ ->
            tree


updateLeaf : (Leaf -> Leaf) -> Uuid -> Tree -> Tree
updateLeaf updateFn targetUuid =
    let
        updateFn_ treeUuid node =
            if treeUuid == targetUuid then
                updateFn node

            else
                node
    in
    mapLeaves updateFn_


mapLeaves : (Uuid -> Leaf -> Leaf) -> Tree -> Tree
mapLeaves mapLeaf tree =
    case tree.content of
        TreeContentNode node ->
            case node.content of
                NodeContentCondition condition ->
                    { tree
                        | content =
                            TreeContentNode
                                { node
                                    | content =
                                        NodeContentCondition
                                            { condition
                                                | containsPositive = mapLeaves mapLeaf condition.containsPositive
                                                , containsNegative = mapLeaves mapLeaf condition.containsNegative
                                            }
                                }
                    }

                _ ->
                    { tree | content = TreeContentNode { node | contains = List.map (mapLeaves mapLeaf) node.contains } }

        TreeContentLeaf leaf ->
            { tree | content = TreeContentLeaf (mapLeaf tree.uuid leaf) }


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
                        , viewDataComponent model dataComponent
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
        , viewComponentDeleteModal model
        ]


viewComponentList : Model -> Html Msg
viewComponentList model =
    let
        viewDataComponentLink component =
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
            (List.map viewDataComponentLink (List.sortBy .name model.app.components)
                ++ [ li [ class "mt-3" ]
                        [ a [ onClick (OpenAddDataComponentModal True) ]
                            [ span [ class "fa-li" ] [ fas "fa-plus" [] ]
                            , text "Add"
                            ]
                        ]
                   ]
            )
        ]


viewDataComponent : Model -> DataComponent -> Html Msg
viewDataComponent model dataComponent =
    div [] [ viewComponent False model dataComponent.content ]


viewComponent : Bool -> Model -> Tree -> Html Msg
viewComponent controls model tree =
    case tree.content of
        TreeContentLeaf leaf ->
            viewLeaf model tree.uuid leaf

        TreeContentNode node ->
            viewNode controls model tree.uuid node


viewNode : Bool -> Model -> Uuid -> Node -> Html Msg
viewNode controls model uuid node =
    let
        dropdownId =
            "node-" ++ Uuid.toString uuid

        dropdownState =
            Maybe.withDefault Dropdown.initialState <|
                Dict.get dropdownId model.dropdownStates

        ( componentIcon, componentName ) =
            case node.content of
                NodeContentStrong ->
                    ( "fas fa-bold", "Strong" )

                NodeContentEmphasis ->
                    ( "fas fa-italic", "Emphasis" )

                NodeContentHeading ->
                    ( "fas fa-heading", "Heading" )

                NodeContentCondition _ ->
                    ( "fas fa-code-branch", "Condition" )

                NodeContentContainer ->
                    ( "fas fa-th-large", "Container" )

                NodeContentIterativeContainer _ ->
                    ( "fas fa-sync-alt", "Iterative Container" )

        dropdownItem icon label msg =
            Dropdown.buttonItem [ onClick msg ] [ fa icon [ class "me-2 text-muted" ], text label ]

        dropdown =
            Dropdown.dropdown dropdownState
                { options = [ Dropdown.attrs [ class "dropdown-text" ] ]
                , toggleMsg = DropdownMsg dropdownId
                , toggleButton =
                    Dropdown.toggle [ Button.roleLink ]
                        [ fa componentIcon [ class "me-1" ]
                        , text componentName
                        ]
                , items =
                    [ dropdownItem "fas fa-th-large" "Container" (UpdateNodeType uuid NodeTypeContainer)
                    , dropdownItem "fas fa-sync-alt" "Iterative Container" (UpdateNodeType uuid NodeTypeIterativeContainer)
                    , dropdownItem "fas fa-code-branch" "Condition" (UpdateNodeType uuid NodeTypeCondition)
                    , dropdownItem "fas fa-bold" "Strong" (UpdateNodeType uuid NodeTypeStrong)
                    , dropdownItem "fas fa-italic" "Emphasis" (UpdateNodeType uuid NodeTypeEmphasis)
                    , dropdownItem "fas fa-heading" "Heading" (UpdateNodeType uuid NodeTypeHeading)
                    ]
                }

        isBlock =
            node.isBlock

        blockCheckbox =
            label [ class "ms-3 cursor-pointer" ]
                [ input [ type_ "checkbox", checked isBlock, onCheck (UpdateIsBlock uuid), class "me-1" ] []
                , text "Block"
                ]

        viewContentComponents =
            [ div [] (List.map (viewComponent True model) (sortTrees node.contains))
            , div [ class "mt-3" ]
                [ button [ class "btn btn-outline-secondary btn-sm me-2", onClick (AddNode uuid) ] [ text "Add node" ]
                , button [ class "btn btn-outline-secondary btn-sm", onClick (AddLeaf uuid) ] [ text "Add leaf" ]
                ]
            ]

        body =
            case node.content of
                NodeContentCondition condition ->
                    [ viewInput { label = "Predicate", value = condition.predicate, onInput = UpdateConditionPredicate uuid }
                    , viewInput { label = "Value", value = condition.value, onInput = UpdateConditionValue uuid }
                    , hr [] []
                    , strong [ class "d-block mb-1" ] [ text "Positive Content" ]
                    , viewComponent False model condition.containsPositive
                    , hr [] []
                    , strong [ class "d-block mb-1" ] [ text "Negative Content" ]
                    , viewComponent False model condition.containsNegative
                    ]

                NodeContentIterativeContainer ic ->
                    [ viewInput { label = "Predicate", value = ic.predicate, onInput = UpdateIterativeContainerPredicate uuid }
                    , hr [] []
                    ]
                        ++ viewContentComponents

                _ ->
                    viewContentComponents
    in
    viewCard model
        { uuid = uuid
        , controls = controls
        , header = [ dropdown, blockCheckbox ]
        , body = body
        }


viewLeaf : Model -> Uuid -> Leaf -> Html Msg
viewLeaf model uuid leaf =
    let
        dropdownId =
            "leaf-" ++ Uuid.toString uuid

        dropdownState =
            Maybe.withDefault Dropdown.initialState <|
                Dict.get dropdownId model.dropdownStates

        ( componentIcon, componentName ) =
            case leaf.content of
                LeafContentContent _ ->
                    ( "fas fa-align-left", "Content" )

                LeafContentDataComponentWrapper _ ->
                    ( "fas fa-box", "Data Component" )

        dropdownItem icon label msg =
            Dropdown.buttonItem [ onClick msg ] [ fa icon [ class "me-2 text-muted" ], text label ]

        dropdown =
            Dropdown.dropdown dropdownState
                { options = [ Dropdown.attrs [ class "dropdown-text" ] ]
                , toggleMsg = DropdownMsg dropdownId
                , toggleButton =
                    Dropdown.toggle [ Button.roleLink ]
                        [ fa componentIcon [ class "me-1" ]
                        , text componentName
                        ]
                , items =
                    [ dropdownItem "fas fa-align-left" "Content" (UpdateLeafType uuid LeafTypeContent)
                    , dropdownItem "fas fa-box" "Data Component" (UpdateLeafType uuid LeafTypeDataComponentWrapper)
                    ]
                }

        body =
            case leaf.content of
                LeafContentContent leafContent ->
                    let
                        sourceDropdownId =
                            "leaf-source-" ++ Uuid.toString uuid

                        sourceDropdownState =
                            Maybe.withDefault Dropdown.initialState <|
                                Dict.get sourceDropdownId model.dropdownStates

                        ( sourceLabel, sourceValue, toSource ) =
                            case leafContent.contentSource of
                                LeafContentSourcePredicate value ->
                                    ( "Predicate", value, LeafContentSourcePredicate )

                                LeafContentSourceText value ->
                                    ( "Text", value, LeafContentSourceText )

                        sourceDropdownItem label contentSource =
                            Dropdown.buttonItem
                                [ onClick (UpdateLeafContentSource uuid (contentSource sourceValue)) ]
                                [ text label ]

                        sourceDropdown =
                            Dropdown.dropdown sourceDropdownState
                                { options = [ Dropdown.attrs [ class "dropdown-text" ] ]
                                , toggleMsg = DropdownMsg sourceDropdownId
                                , toggleButton =
                                    Dropdown.toggle [ Button.roleLink ]
                                        [ text sourceLabel ]
                                , items =
                                    [ sourceDropdownItem "Predicate" LeafContentSourcePredicate
                                    , sourceDropdownItem "Text" LeafContentSourceText
                                    ]
                                }

                        viewContentOption contentContent =
                            option
                                [ value (contentContentToString contentContent)
                                , selected (contentContent == leafContent.content)
                                ]
                                [ text (contentContentToString contentContent) ]

                        contentOptions =
                            [ ContentContent
                            , ContentDate
                            , ContentDateTime
                            , ContentTime
                            , ContentEmail
                            , ContentUrl
                            ]
                    in
                    [ div [ class "form-group row mb-2" ]
                        [ label [ class "col-md-2 col-form-label" ] [ text "Content" ]
                        , div [ class "col-md-10" ]
                            [ select
                                [ class "form-control"
                                , value (contentContentToString leafContent.content)
                                , onInput (UpdateLeafContentContent uuid)
                                ]
                                (List.map viewContentOption contentOptions)
                            ]
                        ]
                    , div [ class "form-group row mb-2" ]
                        [ label [ class "col-md-2 col-form-label" ] [ sourceDropdown ]
                        , div [ class "col-md-10" ]
                            [ input
                                [ type_ "text"
                                , class "form-control"
                                , onInput (UpdateLeafContentSource uuid << toSource)
                                , value sourceValue
                                ]
                                []
                            ]
                        ]
                    ]

                LeafContentDataComponentWrapper dataComponent ->
                    [ viewInput { label = "Predicate", value = dataComponent.predicate, onInput = UpdateDataComponentPredicate uuid }
                    , viewInput { label = "IRI", value = dataComponent.dataComponent, onInput = UpdateDataComponentDataComponent uuid }
                    ]
    in
    viewCard model
        { uuid = uuid
        , controls = True
        , header = [ dropdown ]
        , body = body
        }


type alias ViewCardConfig =
    { uuid : Uuid
    , controls : Bool
    , header : List (Html Msg)
    , body : List (Html Msg)
    }


viewCard : Model -> ViewCardConfig -> Html Msg
viewCard model cfg =
    let
        isCollapsed =
            Set.member (Uuid.toString cfg.uuid) model.collapsedComponents

        collapseButton =
            if isCollapsed then
                a [ onClick (ExpandComponent cfg.uuid), class "text-primary me-3" ] [ fas "fa-chevron-right fa-fw" [] ]

            else
                a [ onClick (CollapseComponent cfg.uuid), class "text-primary me-3" ] [ fas "fa-chevron-down fa-fw" [] ]

        deleteButton =
            a [ onClick (DeleteTree cfg.uuid), class "text-danger ms-3" ]
                [ fas "fa-trash" [] ]

        controlButtons =
            if cfg.controls then
                div [] [ deleteButton ]

            else
                emptyNode

        body =
            if isCollapsed then
                emptyNode

            else
                div [ class "card-body" ] cfg.body
    in
    div [ class "card" ]
        [ div [ class "card-header d-flex justify-content-between" ]
            [ div [] (collapseButton :: cfg.header)
            , div [ class "d-flex align-items-center" ]
                [ small [ class "text-muted" ] [ text (String.replace "-" "" (Uuid.toString cfg.uuid)) ]
                , controlButtons
                ]
            ]
        , body
        ]


type alias ViewInputConfig =
    { label : String
    , value : String
    , onInput : String -> Msg
    }


viewInput : ViewInputConfig -> Html Msg
viewInput cfg =
    div [ class "form-group row mb-2" ]
        [ label [ class "col-md-2 col-form-label" ] [ text cfg.label ]
        , div [ class "col-md-10" ]
            [ input
                [ type_ "text"
                , class "form-control"
                , onInput cfg.onInput
                , value cfg.value
                ]
                []
            ]
        ]



---


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


viewComponentDeleteModal : Model -> Html Msg
viewComponentDeleteModal model =
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
