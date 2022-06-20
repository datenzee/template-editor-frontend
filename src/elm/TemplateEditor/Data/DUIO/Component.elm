module TemplateEditor.Data.DUIO.Component exposing
    ( Component(..)
    , Container
    , IterativeContainer
    , PlainTextComponent(..)
    , PlainTextComponentData
    , TitleComponent
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
import Uuid exposing (Uuid)



-- Component


type Component
    = ContainerComponent Container
    | IterativeContainerComponent IterativeContainer
    | PlainTextComponentComponent PlainTextComponent
    | TitleComponentComponent TitleComponent


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

        "PlainTextComponent" ->
            D.map PlainTextComponentComponent plainTextComponentDecoder

        "TitleComponent" ->
            D.map TitleComponentComponent titleComponentDecoder

        _ ->
            D.fail <| "Unknown component type: " ++ componentType


encode : Component -> E.Value
encode component =
    case component of
        ContainerComponent container ->
            containerEncode container

        IterativeContainerComponent iterativeContainer ->
            iterativeContainerEncode iterativeContainer

        PlainTextComponentComponent plainTextComponent ->
            plainTextComponentEncode plainTextComponent

        TitleComponentComponent titleComponent ->
            titleComponentEncode titleComponent


getUuid : Component -> Uuid
getUuid component =
    case component of
        ContainerComponent container ->
            container.uuid

        IterativeContainerComponent iterativeContainer ->
            iterativeContainer.uuid

        PlainTextComponentComponent plainTextComponent ->
            case plainTextComponent of
                HeadingComponent headingComponent ->
                    headingComponent.uuid

                ParagraphComponent paragraphComponent ->
                    paragraphComponent.uuid

        TitleComponentComponent titleComponent ->
            titleComponent.uuid


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

        PlainTextComponentComponent plainTextComponent ->
            case plainTextComponent of
                HeadingComponent plainTextData ->
                    toIdentifier "HeadingComponent" plainTextData.uuid

                ParagraphComponent plainTextData ->
                    toIdentifier "ParagraphComponent" plainTextData.uuid

        TitleComponentComponent titleComponent ->
            toIdentifier "TitleComponent" titleComponent.uuid


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
                listIdentifier =
                    identifier ++ "_List_"

                toListEntity idx componentId =
                    let
                        firstAndRest =
                            if List.length container.contains == idx + 1 then
                                "    duio:containerListFirst :" ++ componentId ++ " ."

                            else
                                ("    duio:containerListFirst :" ++ componentId ++ " ; \n")
                                    ++ ("    duio:containerListRest :" ++ listIdentifier ++ String.fromInt (idx + 1) ++ " . ")
                    in
                    (":" ++ listIdentifier ++ String.fromInt idx ++ " rdf:type owl:NamedIndividual , duio:ContainerList ;\n")
                        ++ firstAndRest
                        ++ "\n\n"

                containerListComponents =
                    List.map rdfIdentifier container.contains
                        |> List.indexedMap toListEntity
                        |> String.join ""

                containerRdf =
                    """:""" ++ identifier ++ """ rdf:type owl:NamedIndividual , duio:Container ;
    duio:containerContains :""" ++ listIdentifier ++ """0 .                    

"""

                containerComponents =
                    String.join "" <|
                        List.map toRdf container.contains
            in
            containerRdf ++ containerListComponents ++ containerComponents

        IterativeContainerComponent iterativeContainer ->
            let
                iterativeContainerRdf =
                    """:""" ++ identifier ++ """ rdf:type owl:NamedIndividual , duio:IterativeContainer ;
    duio:iterativeContainerContent :""" ++ rdfIdentifier (ContainerComponent iterativeContainer.content) ++ """ ;
    duio:iterativeContainerPredicate <""" ++ iterativeContainer.predicate ++ """> .

"""

                contentRdf =
                    toRdf (ContainerComponent iterativeContainer.content)
            in
            iterativeContainerRdf ++ contentRdf

        PlainTextComponentComponent plainTextComponent ->
            let
                ( data, componentName ) =
                    case plainTextComponent of
                        HeadingComponent headingData ->
                            ( headingData, "HeadingComponent" )

                        ParagraphComponent paragraphData ->
                            ( paragraphData, "ParagraphComponent" )
            in
            """:""" ++ identifier ++ """ rdf:type owl:NamedIndividual , duio:""" ++ componentName ++ """ ;
    duio:plainTextComponentContent \"""" ++ data.content ++ """"^^xsd:string .

"""

        TitleComponentComponent data ->
            """:""" ++ identifier ++ """ rdf:type owl:NamedIndividual , duio:TitleComponent ;
    duio:titleComponentPredicate <""" ++ data.predicate ++ """> .

"""



-- Container


type alias Container =
    { uuid : Uuid
    , contains : List Component
    }


containerDecoder : Decoder Container
containerDecoder =
    D.succeed Container
        |> D.required "uuid" Uuid.decoder
        |> D.required "contains" (D.list decoder)


containerEncode : Container -> E.Value
containerEncode container =
    E.object
        [ ( "type", E.string "Container" )
        , ( "uuid", Uuid.encode container.uuid )
        , ( "contains", E.list encode container.contains )
        ]



-- Iterative Container


type alias IterativeContainer =
    { uuid : Uuid
    , predicate : String
    , content : Container
    }


iterativeContainerDecoder : Decoder IterativeContainer
iterativeContainerDecoder =
    D.succeed IterativeContainer
        |> D.required "uuid" Uuid.decoder
        |> D.required "predicate" D.string
        |> D.required "content" containerDecoder


iterativeContainerEncode : IterativeContainer -> E.Value
iterativeContainerEncode container =
    E.object
        [ ( "type", E.string "IterativeContainer" )
        , ( "uuid", Uuid.encode container.uuid )
        , ( "predicate", E.string container.predicate )
        , ( "content", containerEncode container.content )
        ]



-- Plain Text Component


type PlainTextComponent
    = HeadingComponent PlainTextComponentData
    | ParagraphComponent PlainTextComponentData


type alias PlainTextComponentData =
    { uuid : Uuid
    , content : String
    }


plainTextComponentDecoder : Decoder PlainTextComponent
plainTextComponentDecoder =
    D.field "plainTextType" D.string
        |> D.andThen plainTextComponentDecoderByType


plainTextComponentDecoderByType : String -> Decoder PlainTextComponent
plainTextComponentDecoderByType componentType =
    case componentType of
        "HeadingComponent" ->
            D.map HeadingComponent plainTextComponentDataDecoder

        "ParagraphComponent" ->
            D.map ParagraphComponent plainTextComponentDataDecoder

        _ ->
            D.fail <| "Unknown plain text component type " ++ componentType


plainTextComponentDataDecoder : Decoder PlainTextComponentData
plainTextComponentDataDecoder =
    D.succeed PlainTextComponentData
        |> D.required "uuid" Uuid.decoder
        |> D.required "content" D.string


plainTextComponentEncode : PlainTextComponent -> E.Value
plainTextComponentEncode component =
    let
        encodeComponent plainTextType data =
            E.object
                [ ( "type", E.string "PlainTextComponent" )
                , ( "uuid", Uuid.encode data.uuid )
                , ( "plainTextType", E.string plainTextType )
                , ( "content", E.string data.content )
                ]
    in
    case component of
        HeadingComponent data ->
            encodeComponent "HeadingComponent" data

        ParagraphComponent data ->
            encodeComponent "ParagraphComponent" data



-- Title Component


type alias TitleComponent =
    { uuid : Uuid
    , predicate : String
    }


titleComponentDecoder : Decoder TitleComponent
titleComponentDecoder =
    D.succeed TitleComponent
        |> D.required "uuid" Uuid.decoder
        |> D.required "predicate" D.string


titleComponentEncode : TitleComponent -> E.Value
titleComponentEncode component =
    E.object
        [ ( "type", E.string "TitleComponent" )
        , ( "uuid", Uuid.encode component.uuid )
        , ( "predicate", E.string component.predicate )
        ]
