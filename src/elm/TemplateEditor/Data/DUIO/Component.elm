module TemplateEditor.Data.DUIO.Component exposing
    ( Component(..)
    , Condition
    , Container
    , ContentComponent
    , ContentComponentContent(..)
    , ContentComponentType(..)
    , IterativeContainer
    , decoder
    , defaultContentComponentContent
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
    | ConditionComponent Condition


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

        "Condition" ->
            D.map ConditionComponent conditionDecoder

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

        ConditionComponent condition ->
            conditionEncode condition

        ContentComponentComponent contentComponent ->
            contentComponentEncode contentComponent


getUuid : Component -> Uuid
getUuid component =
    case component of
        ContainerComponent container ->
            container.uuid

        IterativeContainerComponent iterativeContainer ->
            iterativeContainer.uuid

        ConditionComponent condition ->
            condition.uuid

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

        ConditionComponent condition ->
            toIdentifier "Condition" condition.uuid

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

                URLContentComponentType ->
                    toIdentifier "URLComponent" contentComponent.uuid

                EmailContentComponentType ->
                    toIdentifier "EmailComponent" contentComponent.uuid

                DateContentComponentType ->
                    toIdentifier "DateComponent" contentComponent.uuid

                DateTimeContentComponentType ->
                    toIdentifier "DateTimeComponent" contentComponent.uuid

                TimeContentComponentType ->
                    toIdentifier "TimeComponent" contentComponent.uuid


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
                content =
                    ContainerComponent iterativeContainer.content

                iterativeContainerRdf =
                    Rdf.createNode (base identifier)
                        |> Rdf.addPredicate (rdf "type") (owl "NamedIndividual")
                        |> Rdf.addPredicate (rdf "type") (duio "IterativeContainer")
                        |> Rdf.addPredicateBoolean (duio "componentIsBlock") iterativeContainer.isBlock
                        |> Rdf.addPredicate (duio "iterativeContainerContent") (base (rdfIdentifier content))
                        |> Rdf.addPredicateIRI (duio "iterativeContainerPredicate") iterativeContainer.predicate
                        |> Rdf.nodeToString

                contentRdf =
                    toRdf content
            in
            iterativeContainerRdf ++ contentRdf

        ConditionComponent condition ->
            let
                positiveContent =
                    ContainerComponent condition.positiveContent

                negativeContent =
                    ContainerComponent condition.negativeContent

                conditionRdf =
                    Rdf.createNode (base identifier)
                        |> Rdf.addPredicate (rdf "type") (owl "NamedIndividual")
                        |> Rdf.addPredicate (rdf "type") (duio "ConditionComponent")
                        |> Rdf.addPredicateBoolean (duio "componentIsBlock") condition.isBlock
                        |> Rdf.addPredicateIRI (duio "conditionComponentPredicate") condition.predicate
                        |> Rdf.addPredicateLiteral (duio "conditionComponentValue") condition.value
                        |> Rdf.addPredicate (duio "conditionComponentPositiveContent") (base (rdfIdentifier positiveContent))
                        |> Rdf.addPredicate (duio "conditionComponentNegativeContent") (base (rdfIdentifier negativeContent))
                        |> Rdf.nodeToString

                positiveContentRdf =
                    toRdf positiveContent

                negativeContentRdf =
                    toRdf negativeContent
            in
            conditionRdf ++ positiveContentRdf ++ negativeContentRdf

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

                        URLContentComponentType ->
                            "URLComponent"

                        EmailContentComponentType ->
                            "EmailComponent"

                        DateContentComponentType ->
                            "DateComponent"

                        DateTimeContentComponentType ->
                            "DateTimeComponent"

                        TimeContentComponentType ->
                            "TimeComponent"

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

                ( urlLabelProperty, urlLabelValue, urlLabelRdf ) =
                    case ( contentComponent.componentType, contentComponent.urlLabel ) of
                        ( URLContentComponentType, ContentComponentPredicate predicate ) ->
                            ( Just (duio "urlComponentLabelPredicate"), Just (Rdf.IRI predicate), "" )

                        ( URLContentComponentType, ContentComponentText urlLabelText ) ->
                            let
                                urlLabelIdentifier =
                                    identifier ++ "_UrlLabelTextContent"

                                urlLabelTextContentRdf =
                                    Rdf.createNode (base urlLabelIdentifier)
                                        |> Rdf.addPredicateLiteral (duio "textContentValue") urlLabelText
                                        |> Rdf.nodeToString
                            in
                            ( Just (duio "urlComponentLabelContent"), Just (Rdf.Ref (base urlLabelIdentifier)), urlLabelTextContentRdf )

                        _ ->
                            ( Nothing, Nothing, "" )

                rdfAddMaybePredicateObject mbContentProperty mbContentValue rdf =
                    case ( mbContentProperty, mbContentValue ) of
                        ( Just prop, Just val ) ->
                            Rdf.addPredicateObject prop val rdf

                        _ ->
                            rdf

                contentComponentRdf =
                    Rdf.createNode (base identifier)
                        |> Rdf.addPredicate (rdf "type") (owl "NamedIndividual")
                        |> Rdf.addPredicate (rdf "type") (duio componentName)
                        |> Rdf.addPredicateBoolean (duio "componentIsBlock") contentComponent.isBlock
                        |> Rdf.addPredicateObject contentProperty contentValue
                        |> rdfAddMaybePredicateObject urlLabelProperty urlLabelValue
                        |> Rdf.nodeToString
            in
            contentComponentRdf ++ contentRdf ++ urlLabelRdf


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



-- Condition Component


type alias Condition =
    { uuid : Uuid
    , predicate : String
    , value : String
    , positiveContent : Container
    , negativeContent : Container
    , isBlock : Bool
    }


conditionDecoder : Decoder Condition
conditionDecoder =
    D.succeed Condition
        |> D.required "uuid" Uuid.decoder
        |> D.required "predicate" D.string
        |> D.required "value" D.string
        |> D.required "positiveContent" containerDecoder
        |> D.required "negativeContent" containerDecoder
        |> D.required "isBlock" D.bool


conditionEncode : Condition -> E.Value
conditionEncode condition =
    E.object
        [ ( "type", E.string "Condition" )
        , ( "uuid", Uuid.encode condition.uuid )
        , ( "predicate", E.string condition.predicate )
        , ( "value", E.string condition.value )
        , ( "positiveContent", containerEncode condition.positiveContent )
        , ( "negativeContent", containerEncode condition.negativeContent )
        , ( "isBlock", E.bool condition.isBlock )
        ]



-- Content Component


type alias ContentComponent =
    { uuid : Uuid
    , componentType : ContentComponentType
    , content : ContentComponentContent
    , urlLabel : ContentComponentContent
    , isBlock : Bool
    }


type ContentComponentType
    = HeadingContentComponentType
    | TextContentComponentType
    | StrongContentComponentType
    | EmphasisContentComponentType
    | URLContentComponentType
    | EmailContentComponentType
    | DateContentComponentType
    | DateTimeContentComponentType
    | TimeContentComponentType


type ContentComponentContent
    = ContentComponentPredicate String
    | ContentComponentText String


defaultContentComponentContent : ContentComponentContent
defaultContentComponentContent =
    ContentComponentPredicate ""


contentComponentDecoder : Decoder ContentComponent
contentComponentDecoder =
    D.succeed ContentComponent
        |> D.required "uuid" Uuid.decoder
        |> D.required "componentType" contentComponentTypeDecoder
        |> D.required "content" contentComponentContentDecoder
        |> D.optional "urlLabel" contentComponentContentDecoder defaultContentComponentContent
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

                "URLContentComponentType" ->
                    D.succeed URLContentComponentType

                "EmailContentComponentType" ->
                    D.succeed EmailContentComponentType

                "DateContentComponentType" ->
                    D.succeed DateContentComponentType

                "DateTimeContentComponentType" ->
                    D.succeed DateTimeContentComponentType

                "TimeContentComponentType" ->
                    D.succeed TimeContentComponentType

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
        , ( "urlLabel", contentComponentContentEncode component.urlLabel )
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

        URLContentComponentType ->
            E.object
                [ ( "type", E.string "URLContentComponentType" )
                ]

        EmailContentComponentType ->
            E.object
                [ ( "type", E.string "EmailContentComponentType" )
                ]

        DateContentComponentType ->
            E.object
                [ ( "type", E.string "DateContentComponentType" )
                ]

        DateTimeContentComponentType ->
            E.object
                [ ( "type", E.string "DateTimeContentComponentType" )
                ]

        TimeContentComponentType ->
            E.object
                [ ( "type", E.string "TimeContentComponentType" )
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
