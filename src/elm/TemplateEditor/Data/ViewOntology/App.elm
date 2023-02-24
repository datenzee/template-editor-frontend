module TemplateEditor.Data.ViewOntology.App exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Random exposing (Seed)
import Rdf
import TemplateEditor.Data.ViewOntology.Component exposing (DataComponent, dataComponentDecoder, dataComponentToRdf, encodeDataComponent, initDataComponent)
import TemplateEditor.Data.ViewOntology.Prefixes as Prefixes
import Time


type alias App =
    { components : List DataComponent }


decoder : Decoder App
decoder =
    D.succeed App
        |> D.required "components" (D.list dataComponentDecoder)


encode : App -> E.Value
encode app =
    E.object
        [ ( "components", E.list encodeDataComponent app.components ) ]


type alias ToRdfConfig =
    { basePrefix : String
    , license : String
    , time : Time.Posix
    , userName : String
    , userEmail : String
    }


toRdf : App -> ToRdfConfig -> String
toRdf app cfg =
    let
        prefixes =
            List.map Rdf.prefixToString
                [ Prefixes.dctPrefix
                , Prefixes.foafPrefix
                , Prefixes.owlPrefix
                , Prefixes.rdfPrefix
                , Prefixes.rdfsPrefix
                , Prefixes.voPrefix
                , Prefixes.xsdPrefix
                , Rdf.createPrefix "" (cfg.basePrefix ++ "#")
                ]
                ++ [ "@base <" ++ cfg.basePrefix ++ "> ." ]
                |> String.join "\n"

        dataComponents =
            List.map
                (dataComponentToRdf cfg)
                app.components
                |> String.join "\n"
    in
    prefixes ++ "\n\n\n" ++ dataComponents


addDataComponent : Seed -> String -> App -> ( App, Seed )
addDataComponent seed name app =
    let
        ( component, newSeed ) =
            initDataComponent seed name
    in
    ( { app | components = app.components ++ [ component ] }
    , newSeed
    )


updateDataComponent : String -> DataComponent -> App -> App
updateDataComponent componentName newValue app =
    let
        mapComponent component =
            if component.name == componentName then
                newValue

            else
                component
    in
    { app | components = List.map mapComponent app.components }


deleteDataComponent : String -> App -> App
deleteDataComponent name app =
    { app | components = List.filter ((/=) name << .name) app.components }


componentExists : String -> App -> Bool
componentExists name app =
    List.any ((==) name << .name) app.components
