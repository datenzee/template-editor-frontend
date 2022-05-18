module Html.Events.Extra exposing (onLinkClick)

import Html exposing (Attribute)
import Html.Events exposing (custom)
import Json.Decode as D


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }
    in
    custom "click" (D.succeed options)
