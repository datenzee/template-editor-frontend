module TemplateEditor.Api.DSW.Tokens exposing (fetchToken)

import Json.Encode as E
import TemplateEditor.Api.Api exposing (ServerInfo, ToMsg, httpFetch)
import TemplateEditor.Api.DSW.Data.Token as Token exposing (Token)
import TemplateEditor.Data.AppState as AppState exposing (AppState)


fetchToken : String -> String -> AppState -> ToMsg Token msg -> Cmd msg
fetchToken email password appState =
    let
        payload =
            E.object
                [ ( "email", E.string email )
                , ( "password", E.string password )
                ]

        serverInfo =
            AppState.toDSWServerInfo appState
    in
    httpFetch "/tokens" Token.decoder payload serverInfo
