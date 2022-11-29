module TemplateEditor.Data.ViewOntology.Component exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Random exposing (Seed)
import Rdf
import TemplateEditor.Data.DUIO.Prefixes exposing (base, duio, owl, rdf)
import Uuid exposing (Uuid)


type alias DataComponent =
    { name : String
    , content : Tree
    }


initDataComponent : Seed -> String -> ( DataComponent, Seed )
initDataComponent seed name =
    let
        ( container, newSeed ) =
            initTreeContainer seed
    in
    ( { name = name
      , content = container
      }
    , newSeed
    )


dataComponentDecoder : Decoder DataComponent
dataComponentDecoder =
    D.succeed DataComponent
        |> D.required "name" D.string
        |> D.required "content" treeDecoder


encodeDataComponent : DataComponent -> E.Value
encodeDataComponent data =
    E.object
        [ ( "name", E.string data.name )
        , ( "content", encodeTree data.content )
        ]


type alias Tree =
    { order : Float
    , uuid : Uuid
    , content : TreeContent
    }


initTreeContainer : Seed -> ( Tree, Seed )
initTreeContainer seed =
    let
        ( uuid, newSeed ) =
            Random.step Uuid.uuidGenerator seed
    in
    ( { order = 1
      , uuid = uuid
      , content =
            TreeContentNode
                { contains = []
                , isBlock = True
                , content = NodeContentContainer
                }
      }
    , newSeed
    )


initTreeContent : Seed -> ( Tree, Seed )
initTreeContent seed =
    let
        ( uuid, newSeed ) =
            Random.step Uuid.uuidGenerator seed
    in
    ( { order = 1
      , uuid = uuid
      , content =
            TreeContentLeaf
                { content =
                    LeafContentContent
                        { contentSource = LeafContentSourcePredicate ""
                        , content = ContentContent
                        }
                }
      }
    , newSeed
    )


treeDecoder : Decoder Tree
treeDecoder =
    D.succeed Tree
        |> D.required "order" D.float
        |> D.required "uuid" Uuid.decoder
        |> D.required "content" treeContentDecoder


encodeTree : Tree -> E.Value
encodeTree data =
    E.object
        [ ( "order", E.float data.order )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "content", encodeTreeContent data.content )
        ]


sortTrees : List Tree -> List Tree
sortTrees =
    List.sortBy .order


type TreeContent
    = TreeContentLeaf Leaf
    | TreeContentNode Node


treeContentDecoder : Decoder TreeContent
treeContentDecoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "TreeContentLeaf" ->
                        D.map TreeContentLeaf leafDecoder

                    "TreeContentNode" ->
                        D.map TreeContentNode nodeDecoder

                    _ ->
                        D.fail ("Unknown tree content type " ++ type_)
            )


encodeTreeContent : TreeContent -> E.Value
encodeTreeContent data =
    case data of
        TreeContentLeaf leaf ->
            encodeLeaf leaf

        TreeContentNode node ->
            encodeNode node


type alias Leaf =
    { content : LeafContent }


leafDecoder : Decoder Leaf
leafDecoder =
    D.succeed Leaf
        |> D.required "content" leafContentDecoder


encodeLeaf : Leaf -> E.Value
encodeLeaf data =
    E.object
        [ ( "type", E.string "TreeContentLeaf" )
        , ( "content", encodeLeafContent data.content )
        ]


type LeafContent
    = LeafContentDataComponentWrapper DataComponentWrapper
    | LeafContentContent Content


initLeafContentDataComponentWrapper : LeafContent
initLeafContentDataComponentWrapper =
    LeafContentDataComponentWrapper
        { predicate = ""
        , dataComponent = ""
        }


initLeafContentContent : LeafContent
initLeafContentContent =
    LeafContentContent
        { contentSource = LeafContentSourcePredicate ""
        , content = ContentContent
        }


leafContentDecoder : Decoder LeafContent
leafContentDecoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "LeafContentDataComponentWrapper" ->
                        D.map LeafContentDataComponentWrapper dataComponentWrapperDecoder

                    "LeafContentContent" ->
                        D.map LeafContentContent contentDecoder

                    _ ->
                        D.fail ("Unknown leaf content type " ++ type_)
            )


encodeLeafContent : LeafContent -> E.Value
encodeLeafContent data =
    case data of
        LeafContentDataComponentWrapper wrapper ->
            encodeDataComponentWrapper wrapper

        LeafContentContent content ->
            encodeContent content


type alias DataComponentWrapper =
    { predicate : String
    , dataComponent : String
    }


dataComponentWrapperDecoder : Decoder DataComponentWrapper
dataComponentWrapperDecoder =
    D.succeed DataComponentWrapper
        |> D.required "predicate" D.string
        |> D.required "dataComponent" D.string


encodeDataComponentWrapper : DataComponentWrapper -> E.Value
encodeDataComponentWrapper data =
    E.object
        [ ( "type", E.string "LeafContentDataComponentWrapper" )
        , ( "predicate", E.string data.predicate )
        , ( "dataComponent", E.string data.dataComponent )
        ]


type alias Content =
    { contentSource : LeafContentSource
    , content : ContentContent
    }


contentDecoder : Decoder Content
contentDecoder =
    D.succeed Content
        |> D.required "contentSource" leafContentSourceDecoder
        |> D.required "content" contentContentDecoder


encodeContent : Content -> E.Value
encodeContent data =
    E.object
        [ ( "type", E.string "LeafContentContent" )
        , ( "contentSource", encodeLeafContentSource data.contentSource )
        , ( "content", encodeContentContent data.content )
        ]


type LeafContentSource
    = LeafContentSourcePredicate String
    | LeafContentSourceText String


leafContentSourceDecoder : Decoder LeafContentSource
leafContentSourceDecoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "LeafContentSourcePredicate" ->
                        D.map LeafContentSourcePredicate (D.field "predicate" D.string)

                    "LeafContentSourceText" ->
                        D.map LeafContentSourceText (D.field "text" D.string)

                    _ ->
                        D.fail ("Unknown content source" ++ type_)
            )


encodeLeafContentSource : LeafContentSource -> E.Value
encodeLeafContentSource data =
    case data of
        LeafContentSourcePredicate predicate ->
            E.object
                [ ( "type", E.string "LeafContentSourcePredicate" )
                , ( "predicate", E.string predicate )
                ]

        LeafContentSourceText text ->
            E.object
                [ ( "type", E.string "LeafContentSourceText" )
                , ( "text", E.string text )
                ]


type ContentContent
    = ContentContent
    | ContentDate
    | ContentDateTime
    | ContentEmail
    | ContentTime
    | ContentUrl Url


contentContentDecoder : Decoder ContentContent
contentContentDecoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "ContentContent" ->
                        D.succeed ContentContent

                    "ContentDate" ->
                        D.succeed ContentDate

                    "ContentDateTime" ->
                        D.succeed ContentDateTime

                    "ContentEmail" ->
                        D.succeed ContentEmail

                    "ContentTime" ->
                        D.succeed ContentTime

                    "ContentUrl" ->
                        D.map ContentUrl urlDecoder

                    _ ->
                        D.fail ("Unknown content type " ++ type_)
            )


encodeContentContent : ContentContent -> E.Value
encodeContentContent data =
    case data of
        ContentContent ->
            E.object [ ( "type", E.string "ContentContent" ) ]

        ContentDate ->
            E.object [ ( "type", E.string "ContentDate" ) ]

        ContentDateTime ->
            E.object [ ( "type", E.string "ContentDateTime" ) ]

        ContentEmail ->
            E.object [ ( "type", E.string "ContentEmail" ) ]

        ContentTime ->
            E.object [ ( "type", E.string "ContentTime" ) ]

        ContentUrl url ->
            encodeUrl url


type alias Url =
    { labelContent : TextContent
    , labelPredicate : String
    }


urlDecoder : Decoder Url
urlDecoder =
    D.succeed Url
        |> D.required "labelContent" textContentDecoder
        |> D.required "labelPredicate" D.string


encodeUrl : Url -> E.Value
encodeUrl url =
    E.object
        [ ( "type", E.string "ContentUrl" )
        , ( "labelContent", encodeTextContent url.labelContent )
        , ( "labelPredicate", E.string url.labelPredicate )
        ]


type alias Node =
    { contains : List Tree
    , isBlock : Bool
    , content : NodeContent
    }


nodeDecoder : Decoder Node
nodeDecoder =
    D.succeed Node
        |> D.required "contains" (D.list treeDecoder)
        |> D.required "isBlock" D.bool
        |> D.required "content" nodeContentDecoder


encodeNode : Node -> E.Value
encodeNode data =
    E.object
        [ ( "type", E.string "TreeContentNode" )
        , ( "contains", E.list encodeTree data.contains )
        , ( "isBlock", E.bool data.isBlock )
        , ( "content", encodeNodeContent data.content )
        ]


type NodeContent
    = NodeContentStrong
    | NodeContentEmphasis
    | NodeContentHeading
    | NodeContentCondition Condition
    | NodeContentContainer
    | NodeContentIterativeContainer IterativeContainerData


initNodeContentCondition : Seed -> ( NodeContent, Seed )
initNodeContentCondition seed =
    let
        ( condition, newSeed ) =
            initCondition seed
    in
    ( NodeContentCondition condition, newSeed )


initNodeContentIterativeContainer : NodeContent
initNodeContentIterativeContainer =
    NodeContentIterativeContainer { predicate = "" }


nodeContentDecoder : Decoder NodeContent
nodeContentDecoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "NodeContentStrong" ->
                        D.succeed NodeContentStrong

                    "NodeContentEmphasis" ->
                        D.succeed NodeContentEmphasis

                    "NodeContentHeading" ->
                        D.succeed NodeContentHeading

                    "NodeContentCondition" ->
                        D.map NodeContentCondition conditionDecoder

                    "NodeContentContainer" ->
                        D.succeed NodeContentContainer

                    "NodeContentIterativeContainer" ->
                        D.map NodeContentIterativeContainer iterativeContainerDataDecoder

                    _ ->
                        D.fail ("Unknown node content type " ++ type_)
            )


encodeNodeContent : NodeContent -> E.Value
encodeNodeContent data =
    case data of
        NodeContentStrong ->
            E.object [ ( "type", E.string "NodeContentStrong" ) ]

        NodeContentEmphasis ->
            E.object [ ( "type", E.string "NodeContentEmphasis" ) ]

        NodeContentHeading ->
            E.object [ ( "type", E.string "NodeContentHeading" ) ]

        NodeContentCondition condition ->
            encodeCondition condition

        NodeContentContainer ->
            E.object [ ( "type", E.string "NodeContentContainer" ) ]

        NodeContentIterativeContainer iterativeContainer ->
            encodeIterativeContainerData iterativeContainer


type alias Condition =
    { predicate : String
    , value : String
    , containsPositive : Tree
    , containsNegative : Tree
    }


conditionDecoder : Decoder Condition
conditionDecoder =
    D.succeed Condition
        |> D.required "predicate" D.string
        |> D.required "value" D.string
        |> D.required "containsPositive" treeDecoder
        |> D.required "containsNegative" treeDecoder


encodeCondition : Condition -> E.Value
encodeCondition data =
    E.object
        [ ( "type", E.string "NodeContentCondition" )
        , ( "predicate", E.string data.predicate )
        , ( "value", E.string data.value )
        , ( "containsPositive", encodeTree data.containsPositive )
        , ( "containsNegative", encodeTree data.containsNegative )
        ]


initCondition : Seed -> ( Condition, Seed )
initCondition seed =
    let
        ( positiveContent, newSeed1 ) =
            initTreeContainer seed

        ( negativeContent, newSeed2 ) =
            initTreeContainer newSeed1
    in
    ( { predicate = ""
      , value = ""
      , containsPositive = positiveContent
      , containsNegative = negativeContent
      }
    , newSeed2
    )


type alias IterativeContainerData =
    { predicate : String }


iterativeContainerDataDecoder : Decoder IterativeContainerData
iterativeContainerDataDecoder =
    D.succeed IterativeContainerData
        |> D.required "predicate" D.string


encodeIterativeContainerData : IterativeContainerData -> E.Value
encodeIterativeContainerData data =
    E.object
        [ ( "type", E.string "NodeContentIterativeContainer" )
        , ( "predicate", E.string data.predicate )
        ]


type alias TextContent =
    { value : String }


textContentDecoder : Decoder TextContent
textContentDecoder =
    D.succeed TextContent
        |> D.required "value" D.string


encodeTextContent : TextContent -> E.Value
encodeTextContent data =
    E.object
        [ ( "value", E.string data.value ) ]
