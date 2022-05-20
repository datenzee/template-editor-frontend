module TemplateEditor.Api.TemplateEditor.TemplateEditors exposing (..)

import Json.Decode as D
import TemplateEditor.Api.Api exposing (ToMsg, jwtGet)
import TemplateEditor.Data.AppState as AppState exposing (AppState)
import TemplateEditor.Data.Pagination as Pagination exposing (Pagination)
import TemplateEditor.Data.TemplateEditor as TemplateEditor exposing (TemplateEditor)


getTemplateEditors : AppState -> ToMsg (Pagination TemplateEditor) msg -> Cmd msg
getTemplateEditors appState =
    jwtGet "/template-editors" (Pagination.decoder TemplateEditor.decoder) (AppState.toTEServerInfo appState)
