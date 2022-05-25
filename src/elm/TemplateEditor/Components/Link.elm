module TemplateEditor.Components.Link exposing
    ( linkTo
    , linkToAttributes
    )

import Html exposing (Html, a)
import Html.Attributes exposing (href)
import TemplateEditor.Routes as Routes
import TemplateEditor.Routing as Routing


linkTo : Routes.Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
linkTo route attributes children =
    a (attributes ++ linkToAttributes route) children


linkToAttributes : Routes.Route -> List (Html.Attribute msg)
linkToAttributes route =
    [ href <| Routing.toUrl route
    ]
