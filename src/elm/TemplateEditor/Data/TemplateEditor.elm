module TemplateEditor.Data.TemplateEditor exposing
    ( TemplateEditor
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias TemplateEditor =
    { name : String }


decoder : Decoder TemplateEditor
decoder =
    D.succeed TemplateEditor
        |> D.required "name" D.string


encode : TemplateEditor -> E.Value
encode templateEditor =
    E.object []
