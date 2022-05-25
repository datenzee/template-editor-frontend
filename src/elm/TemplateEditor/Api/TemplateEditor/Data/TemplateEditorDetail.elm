module TemplateEditor.Api.TemplateEditor.Data.TemplateEditorDetail exposing
    ( TemplateEditorDetail
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias TemplateEditorDetail =
    { id : Int
    , name : String
    }


decoder : Decoder TemplateEditorDetail
decoder =
    D.succeed TemplateEditorDetail
        |> D.required "id" D.int
        |> D.required "name" D.string


encode : TemplateEditorDetail -> E.Value
encode templateEditor =
    E.object []
