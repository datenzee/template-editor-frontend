module TemplateEditor.Data.Flags exposing
    ( Flags
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import TemplateEditor.Data.Session as Session exposing (Session)


type alias Flags =
    { session : Maybe Session
    , dswApiUrl : String
    , teApiUrl : String
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "session" (D.nullable Session.decoder)
        |> D.required "dswApiUrl" D.string
        |> D.required "teApiUrl" D.string


default : Flags
default =
    { session = Nothing
    , dswApiUrl = ""
    , teApiUrl = ""
    }
