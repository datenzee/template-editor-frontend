module TemplateEditor.Data.ViewOntology.App exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import TemplateEditor.Data.ViewOntology.Component exposing (DataComponent, dataComponentDecoder, encodeDataComponent, initDataComponent)


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


addDataComponent : String -> App -> App
addDataComponent name app =
    { app | components = app.components ++ [ initDataComponent name ] }


deleteDataComponent : String -> App -> App
deleteDataComponent name app =
    { app | components = List.filter ((/=) name << .name) app.components }


componentExists : String -> App -> Bool
componentExists name app =
    List.any ((==) name << .name) app.components
