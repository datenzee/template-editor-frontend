module TemplateEditor.Common.FontAwesome exposing (..)

import Html exposing (Html, i)
import Html.Attributes exposing (class)


fas : String -> List (Html.Attribute msg) -> Html msg
fas icon =
    fa ("fas " ++ icon)


far : String -> List (Html.Attribute msg) -> Html msg
far icon =
    fa ("far " ++ icon)


fa : String -> List (Html.Attribute msg) -> Html msg
fa icon attrs =
    i (class icon :: attrs) []
