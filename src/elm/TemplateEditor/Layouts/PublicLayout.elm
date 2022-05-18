module TemplateEditor.Layouts.PublicLayout exposing (..)

import Browser exposing (Document)
import Html exposing (Html)


view : Maybe String -> Html msg -> Document msg
view mbTitle content =
    let
        title =
            case mbTitle of
                Just t ->
                    t ++ " | Template Editor"

                Nothing ->
                    "Template Editor"
    in
    { title = title
    , body =
        [ content ]
    }
