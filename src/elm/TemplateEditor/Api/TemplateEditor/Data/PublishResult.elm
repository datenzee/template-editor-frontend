module TemplateEditor.Api.TemplateEditor.Data.PublishResult exposing (PublishResult, decoder)

import Json.Decode as D
import Json.Decode.Pipeline as D


type alias PublishResult =
    { url : String }


decoder =
    D.succeed PublishResult
        |> D.required "url" D.string
