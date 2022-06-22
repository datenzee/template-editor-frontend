module TemplateEditor.Data.DUIO.App exposing
    ( App
    , decoder
    , encode
    , toRdf
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Rdf
import TemplateEditor.Data.DUIO.Component as Component exposing (Component)
import TemplateEditor.Data.DUIO.Prefixes as Prefixes exposing (base, duio, owl, rdf)


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
            List.map Rdf.prefixToString
                [ Prefixes.duioPrefix
                , Prefixes.owlPrefix
                , Prefixes.rdfPrefix
                , Prefixes.xmlPrefix
                , Prefixes.xsdPrefix
                , Prefixes.rdfsPrefix
                , Prefixes.basePrefix
                ]
                ++ [ "@base <http://example.com> ." ]
                |> String.join "\n"

        rootComponent =
            Component.toRdfOpts True app.rootComponent

        appRdf =
            Rdf.createNode (base "App")
                |> Rdf.addPredicate (rdf "type") (owl "NamedIndividual")
                |> Rdf.addPredicate (rdf "type") (duio "App")
                |> Rdf.addPredicate (duio "rootComponent") (base Component.rootContainerIdentifier)
                |> Rdf.nodeToString
    in
    prefixes ++ "\n\n" ++ appRdf ++ rootComponent
