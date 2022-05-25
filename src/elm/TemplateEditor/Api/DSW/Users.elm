module TemplateEditor.Api.DSW.Users exposing (getCurrentUser)

import TemplateEditor.Api.Api exposing (ToMsg, jwtGet)
import TemplateEditor.Api.DSW.Data.User as User exposing (User)
import TemplateEditor.Data.AppState as AppState exposing (AppState)


getCurrentUser : AppState -> ToMsg User msg -> Cmd msg
getCurrentUser appState =
    jwtGet "/users/current" User.decoder (AppState.toDSWServerInfo appState)
