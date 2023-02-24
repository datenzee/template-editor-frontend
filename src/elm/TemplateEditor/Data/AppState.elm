module TemplateEditor.Data.AppState exposing (AppState, init, toDSWServerInfo, toTEServerInfo)

import Browser.Navigation as Navigation
import Json.Decode as D
import Random exposing (Seed)
import TemplateEditor.Api.Api exposing (ServerInfo)
import TemplateEditor.Data.Flags as Flags
import TemplateEditor.Data.Session as Session exposing (Session)
import TemplateEditor.Routes as Routes
import Time


type alias AppState =
    { route : Routes.Route
    , navigationKey : Navigation.Key
    , dswApiUrl : String
    , teApiUrl : String
    , session : Session
    , invalidSession : Bool
    , seed : Seed
    , currentTime : Time.Posix
    }


init : D.Value -> Routes.Route -> Navigation.Key -> AppState
init flagsValue route navigationKey =
    let
        flagsResult =
            D.decodeValue Flags.decoder flagsValue

        flags =
            Result.withDefault Flags.default flagsResult

        invalidSession =
            case flagsResult of
                Err (D.Field "session" _) ->
                    True

                _ ->
                    False
    in
    { route = route
    , navigationKey = navigationKey
    , dswApiUrl = flags.dswApiUrl
    , teApiUrl = flags.teApiUrl
    , session = Maybe.withDefault Session.init flags.session
    , invalidSession = invalidSession
    , seed = Random.initialSeed flags.seed
    , currentTime = Time.millisToPosix 0
    }


toDSWServerInfo : AppState -> ServerInfo
toDSWServerInfo appState =
    { apiUrl = appState.dswApiUrl
    , token = getToken appState
    }


toTEServerInfo : AppState -> ServerInfo
toTEServerInfo appState =
    { apiUrl = appState.teApiUrl ++ "/api"
    , token = getToken appState
    }


getToken : AppState -> Maybe String
getToken appState =
    if String.isEmpty appState.session.token.token then
        Nothing

    else
        Just appState.session.token.token
