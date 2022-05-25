module TemplateEditor.Routing exposing (cmdNavigate, matchers, parseLocation, toUrl)

import Browser.Navigation exposing (pushUrl)
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Routes as Routes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, s, top)


matchers : Parser (Routes.Route -> a) a
matchers =
    Parser.oneOf
        [ Parser.map Routes.Login (s "login")
        , Parser.map Routes.Dashboard top
        , Parser.map Routes.TemplateEditor (s "editor" </> int)
        ]


toUrl : Routes.Route -> String
toUrl route =
    let
        parts =
            case route of
                Routes.Login ->
                    [ "login " ]

                Routes.Dashboard ->
                    []

                Routes.TemplateEditor id ->
                    [ "editor", String.fromInt id ]

                _ ->
                    []
    in
    "/"
        ++ String.join "/" parts
        |> String.split "/?"
        |> String.join "?"


parseLocation : Url -> Routes.Route
parseLocation =
    Maybe.withDefault Routes.NotFound << Parser.parse matchers


cmdNavigate : AppState -> Routes.Route -> Cmd msg
cmdNavigate appState =
    pushUrl appState.navigationKey << toUrl
