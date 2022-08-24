module TemplateEditor.Data.DUIO.Component exposing
    ( Component(..)
    , Container
    , ContentComponent
    , ContentComponentContent(..)
    , ContentComponentType(..)
    , IterativeContainer
    , decoder
    , encode
    , getUuid
    , rdfIdentifier
    , rootContainerIdentifier
    , toRdf
    , toRdfOpts
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Rdf
import TemplateEditor.Data.DUIO.Prefixes exposing (base, duio, owl, rdf)
import Uuid exposing (Uuid)



-- Component


type Component
    = ContainerComponent Container
    | IterativeContainerComponent IterativeContainer
    | ContentComponentComponent ContentComponent


decoder : Decoder Component
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder Component
decoderByType componentType =
    case componentType of
        "Container" ->
            D.map ContainerComponent containerDecoder

        "IterativeContainer" ->
            D.map IterativeContainerComponent iterativeContainerDecoder

        "ContentComponent" ->
            D.map ContentComponentComponent contentComponentDecoder

        _ ->
            D.fail <| "Unknown component type: " ++ componentType


encode : Component -> E.Value
encode component =
    case component of
        ContainerComponent container ->
            containerEncode container

        IterativeContainerComponent iterativeContainer ->
            iterativeContainerEncode iterativeContainer

        ContentComponentComponent contentComponent ->
            contentComponentEncode contentComponent


getUuid : Component -> Uuid
getUuid component =
    case component of
        ContainerComponent container ->
            container.uuid

        IterativeContainerComponent iterativeContainer ->
            iterativeContainer.uuid

        ContentComponentComponent contentComponent ->
            contentComponent.uuid


rdfIdentifier : Component -> String
rdfIdentifier component =
    let
        toIdentifier componentName uuid =
            componentName ++ "_" ++ String.replace "-" "" (Uuid.toString uuid)
    in
    case component of
        ContainerComponent container ->
            toIdentifier "Container" container.uuid

        IterativeContainerComponent iterativeContainer ->
            toIdentifier "IterativeContainer" iterativeContainer.uuid

        ContentComponentComponent contentComponent ->
            case contentComponent.componentType of
                HeadingContentComponentType ->
                    toIdentifier "HeadingComponent" contentComponent.uuid

                TextContentComponentType ->
                    toIdentifier "TextComponent" contentComponent.uuid

                StrongContentComponentType ->
                    toIdentifier "StrongComponent" contentComponent.uuid

                EmphasisContentComponentType ->
                    toIdentifier "EmphasisComponent" contentComponent.uuid


rootContainerIdentifier : String
rootContainerIdentifier =
    "Container_Root"


toRdf : Component -> String
toRdf =
    toRdfOpts False


toRdfOpts : Bool -> Component -> String
toRdfOpts isRootContainer component =
    let
        identifier =
            if isRootContainer then
                rootContainerIdentifier

            else
                rdfIdentifier component
    in
    case component of
        ContainerComponent container ->
            let
                listIdentifier idx =
                    identifier ++ "_List_" ++ String.fromInt idx

                toListEntity idx componentId =
                    let
                        listEntity =
                            Rdf.createNode (base (listIdentifier idx))
                                |> Rdf.addPredicate (rdf "type") (owl "NamedIndividual")
                                |> Rdf.addPredicate (rdf "type") (duio "ContainerList")
                                |> Rdf.addPredicate (duio "containerListFirst") (base componentId)

                        listEntityWithRest =
                            if List.length container.contains == idx + 1 then
                                listEntity

                            else
                                listEntity
                                    |> Rdf.addPredicate (duio "containerListRest") (base (listIdentifier (idx + 1)))
                    in
                    Rdf.nodeToString listEntityWithRest

                containerRdf =
                    Rdf.createNode (base identifier)
                        |> Rdf.addPredicate (rdf "type") (owl "NamedIndividual")
                        |> Rdf.addPredicate (rdf "type") (duio "Container")
                        |> Rdf.addPredicateBoolean (duio "componentIsBlock") container.isBlock
                        |> Rdf.addPredicate (duio "containerContains") (base (listIdentifier 0))
                        |> Rdf.nodeToString

                containerListComponents =
                    List.map rdfIdentifier container.contains
                        |> List.indexedMap toListEntity
                        |> String.join ""

                containerComponents =
                    List.map toRdf container.contains
                        |> String.join ""
            in
            containerRdf ++ containerListComponents ++ containerComponents

        IterativeContainerComponent iterativeContainer ->
            let
                iterativeContainerRdf =
                    Rdf.createNode (base identifier)
                        |> Rdf.addPredicate (rdf "type") (owl "NamedIndividual")
                        |> Rdf.addPredicate (rdf "type") (duio "IterativeContainer")
                        |> Rdf.addPredicateBoolean (duio "componentIsBlock") iterativeContainer.isBlock
                        |> Rdf.addPredicate (duio "iterativeContainerContent") (base (rdfIdentifier (ContainerComponent iterativeContainer.content)))
                        |> Rdf.addPredicateIRI (duio "iterativeContainerPredicate") iterativeContainer.predicate
                        |> Rdf.nodeToString

                contentRdf =
                    toRdf (ContainerComponent iterativeContainer.content)
            in
            iterativeContainerRdf ++ contentRdf

        ContentComponentComponent contentComponent ->
            let
                componentName =
                    case contentComponent.componentType of
                        HeadingContentComponentType ->
                            "HeadingComponent"

                        TextContentComponentType ->
                            "TextComponent"

                        StrongContentComponentType ->
                            "StrongComponent"

                        EmphasisContentComponentType ->
                            "EmphasisComponent"

                ( contentProperty, contentValue, contentRdf ) =
                    case contentComponent.content of
                        ContentComponentPredicate predicate ->
                            ( duio "contentComponentPredicate", Rdf.IRI predicate, "" )

                        ContentComponentText contentText ->
                            let
                                textContentIdentifier =
                                    identifier ++ "_TextContent"

                                textContentRdf =
                                    Rdf.createNode (base textContentIdentifier)
                                        |> Rdf.addPredicateLiteral (duio "textContentValue") contentText
                                        |> Rdf.nodeToString
                            in
                            ( duio "contentComponentContent", Rdf.Ref (base textContentIdentifier), textContentRdf )

                contentComponentRdf =
                    Rdf.createNode (base identifier)
                        |> Rdf.addPredicate (rdf "type") (owl "NamedIndividual")
                        |> Rdf.addPredicate (rdf "type") (duio componentName)
                        |> Rdf.addPredicateBoolean (duio "componentIsBlock") contentComponent.isBlock
                        |> Rdf.addPredicateObject contentProperty contentValue
                        |> Rdf.nodeToString
            in
            contentComponentRdf ++ contentRdf


type alias Container =
    { uuid : Uuid
    , contains : List Component
    , isBlock : Bool
    }


containerDecoder : Decoder Container
containerDecoder =
    D.succeed Container
        |> D.required "uuid" Uuid.decoder
        |> D.required "contains" (D.list decoder)
        |> D.required "isBlock" D.bool


containerEncode : Container -> E.Value
containerEncode container =
    E.object
        [ ( "type", E.string "Container" )
        , ( "uuid", Uuid.encode container.uuid )
        , ( "contains", E.list encode container.contains )
        , ( "isBlock", E.bool container.isBlock )
        ]



-- Iterative Container


type alias IterativeContainer =
    { uuid : Uuid
    , predicate : String
    , content : Container
    , isBlock : Bool
    }


iterativeContainerDecoder : Decoder IterativeContainer
iterativeContainerDecoder =
    D.succeed IterativeContainer
        |> D.required "uuid" Uuid.decoder
        |> D.required "predicate" D.string
        |> D.required "content" containerDecoder
        |> D.required "isBlock" D.bool


iterativeContainerEncode : IterativeContainer -> E.Value
iterativeContainerEncode container =
    E.object
        [ ( "type", E.string "IterativeContainer" )
        , ( "uuid", Uuid.encode container.uuid )
        , ( "predicate", E.string container.predicate )
        , ( "content", containerEncode container.content )
        , ( "isBlock", E.bool container.isBlock )
        ]



-- Content Component


type alias ContentComponent =
    { uuid : Uuid
    , componentType : ContentComponentType
    , content : ContentComponentContent
    , isBlock : Bool
    }


type ContentComponentType
    = HeadingContentComponentType
    | TextContentComponentType
    | StrongContentComponentType
    | EmphasisContentComponentType


type ContentComponentContent
    = ContentComponentPredicate String
    | ContentComponentText String


contentComponentDecoder : Decoder ContentComponent
contentComponentDecoder =
    D.succeed ContentComponent
        |> D.required "uuid" Uuid.decoder
        |> D.required "componentType" contentComponentTypeDecoder
        |> D.required "content" contentComponentContentDecoder
        |> D.required "isBlock" D.bool


contentComponentTypeDecoder : Decoder ContentComponentType
contentComponentTypeDecoder =
    let
        decode componentType =
            case componentType of
                "HeadingComponent" ->
                    D.succeed HeadingContentComponentType

                "TextComponent" ->
                    D.succeed TextContentComponentType

                "StrongContentComponentType" ->
                    D.succeed StrongContentComponentType

                "EmphasisContentComponentType" ->
                    D.succeed EmphasisContentComponentType

                _ ->
                    D.fail <| "Unknown plain text component type " ++ componentType
    in
    D.field "type" D.string
        |> D.andThen decode


contentComponentContentDecoder : Decoder ContentComponentContent
contentComponentContentDecoder =
    let
        decode componentType =
            case componentType of
                "ContentComponentPredicate" ->
                    D.map ContentComponentPredicate (D.field "predicate" D.string)

                "ContentComponentText" ->
                    D.map ContentComponentText (D.field "text" D.string)

                _ ->
                    D.fail <| "Unknown plain text component type " ++ componentType
    in
    D.field "type" D.string
        |> D.andThen decode


contentComponentEncode : ContentComponent -> E.Value
contentComponentEncode component =
    E.object
        [ ( "type", E.string "ContentComponent" )
        , ( "uuid", Uuid.encode component.uuid )
        , ( "componentType", contentComponentTypeEncode component.componentType )
        , ( "content", contentComponentContentEncode component.content )
        , ( "isBlock", E.bool component.isBlock )
        ]


contentComponentTypeEncode : ContentComponentType -> E.Value
contentComponentTypeEncode contentComponentType =
    case contentComponentType of
        HeadingContentComponentType ->
            E.object
                [ ( "type", E.string "HeadingComponent" )
                ]

        TextContentComponentType ->
            E.object
                [ ( "type", E.string "TextComponent" )
                ]

        StrongContentComponentType ->
            E.object
                [ ( "type", E.string "StrongContentComponentType" )
                ]

        EmphasisContentComponentType ->
            E.object
                [ ( "type", E.string "EmphasisContentComponentType" )
                ]


contentComponentContentEncode : ContentComponentContent -> E.Value
contentComponentContentEncode contentComponentContent =
    case contentComponentContent of
        ContentComponentPredicate predicate ->
            E.object
                [ ( "type", E.string "ContentComponentPredicate" )
                , ( "predicate", E.string predicate )
                ]

        ContentComponentText text ->
            E.object
                [ ( "type", E.string "ContentComponentText" )
                , ( "text", E.string text )
                ]
