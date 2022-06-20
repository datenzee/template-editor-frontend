module TemplateEditor.Data.DUIO.App exposing
    ( App
    , decoder
    , encode
    , toRdf
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import TemplateEditor.Data.DUIO.Component as Component exposing (Component)


type alias App =
    { rootComponent : Component }


decoder : Decoder App
decoder =
    D.succeed App
        |> D.required "rootComponent" Component.decoder


encode : App -> E.Value
encode app =
    E.object
        [ ( "rootComponent", Component.encode app.rootComponent )
        ]


toRdf : App -> String
toRdf app =
    let
        prefixes =
            """@prefix duio: <http://www.semanticweb.org/janslifka/ontologies/2022/2/datenzee-ui-ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xml: <http://www.w3.org/XML/1998/namespace> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://example.com#> .
@base <http://example.com> .


"""

        appRdf =
            """:App rdf:type owl:NamedIndividual , duio:App ;
    duio:rootComponent :""" ++ Component.rootContainerIdentifier ++ """ .

"""

        rootComponent =
            Component.toRdfOpts True app.rootComponent
    in
    prefixes ++ appRdf ++ rootComponent
