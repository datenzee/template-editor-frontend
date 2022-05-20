module TemplateEditor.Data.Pagination exposing
    ( Pagination
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Pagination a =
    { items : List a }


decoder : Decoder a -> Decoder (Pagination a)
decoder itemDecoder =
    D.succeed Pagination
        |> D.required "entries" (D.list itemDecoder)
