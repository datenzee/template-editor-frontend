module TemplateEditor.Data.DUIO.App exposing
    ( App
    , decoder
    , encode
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
