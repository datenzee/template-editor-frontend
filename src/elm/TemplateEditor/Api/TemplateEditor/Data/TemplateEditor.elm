module TemplateEditor.Api.TemplateEditor.Data.TemplateEditor exposing
    ( TemplateEditor
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias TemplateEditor =
    { id : Int
    , name : String
    }


decoder : Decoder TemplateEditor
decoder =
    D.succeed TemplateEditor
        |> D.required "id" D.int
        |> D.required "name" D.string


encode : TemplateEditor -> E.Value
encode templateEditor =
    E.object
        [ ( "id", E.int templateEditor.id )
        , ( "name", E.string templateEditor.name )
        ]
