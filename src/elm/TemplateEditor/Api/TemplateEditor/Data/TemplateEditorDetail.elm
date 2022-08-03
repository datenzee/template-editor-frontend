module TemplateEditor.Api.TemplateEditor.Data.TemplateEditorDetail exposing
    ( TemplateEditorDetail
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import TemplateEditor.Data.DUIO.App as App exposing (App)


type alias TemplateEditorDetail =
    { id : Int
    , name : String
    , content : App
    , url : Maybe String
    , dataUrl : Maybe String
    }


decoder : Decoder TemplateEditorDetail
decoder =
    let
        contentDecoder =
            D.string
                |> D.andThen
                    (\str ->
                        case D.decodeString App.decoder str of
                            Ok app ->
                                D.succeed app

                            Err _ ->
                                D.fail "Error"
                    )
    in
    D.succeed TemplateEditorDetail
        |> D.required "id" D.int
        |> D.required "name" D.string
        |> D.required "content" contentDecoder
        |> D.optional "url" (D.maybe D.string) Nothing
        |> D.optional "data_url" (D.maybe D.string) Nothing


encode : TemplateEditorDetail -> E.Value
encode templateEditor =
    E.object
        [ ( "name", E.string templateEditor.name )
        , ( "content", E.string <| E.encode 0 <| App.encode templateEditor.content )
        , ( "url", E.maybe E.string templateEditor.url )
        , ( "datUrl", E.maybe E.string templateEditor.url )
        ]
