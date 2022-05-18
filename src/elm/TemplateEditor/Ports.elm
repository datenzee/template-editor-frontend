port module TemplateEditor.Ports exposing (..)

import Json.Encode as E



-- Session


port storeSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg


port clearSessionAndReload : () -> Cmd msg
