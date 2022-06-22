module TemplateEditor.Api.TemplateEditor.TemplateEditors exposing (..)

import Json.Encode as E
import TemplateEditor.Api.Api exposing (ToMsg, jwtFetch, jwtGet, jwtPost, jwtPut)
import TemplateEditor.Api.TemplateEditor.Data.Pagination as Pagination exposing (Pagination)
import TemplateEditor.Api.TemplateEditor.Data.TemplateEditor as TemplateEditor exposing (TemplateEditor)
import TemplateEditor.Api.TemplateEditor.Data.TemplateEditorDetail as TemplateEditorDetail exposing (TemplateEditorDetail)
import TemplateEditor.Data.AppState as AppState exposing (AppState)


getTemplateEditors : AppState -> ToMsg (Pagination TemplateEditor) msg -> Cmd msg
getTemplateEditors appState =
    jwtGet "/template-editors" (Pagination.decoder TemplateEditor.decoder) (AppState.toTEServerInfo appState)


getTemplateEditor : AppState -> Int -> ToMsg TemplateEditorDetail msg -> Cmd msg
getTemplateEditor appState id =
    jwtGet ("/template-editors/" ++ String.fromInt id) TemplateEditorDetail.decoder (AppState.toTEServerInfo appState)


putTemplateEditor : AppState -> Int -> TemplateEditorDetail -> ToMsg () msg -> Cmd msg
putTemplateEditor appState id detail =
    jwtPut ("/template-editors/" ++ String.fromInt id) (TemplateEditorDetail.encode detail) (AppState.toTEServerInfo appState)


publish : AppState -> Int -> String -> ToMsg () msg -> Cmd msg
publish appState id rdf =
    let
        data =
            E.object
                [ ( "rdf", E.string rdf ) ]
    in
    jwtPost ("/template-editors/" ++ String.fromInt id ++ "/expansions-and-publications") data (AppState.toTEServerInfo appState)
