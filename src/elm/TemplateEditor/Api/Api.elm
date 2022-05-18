module TemplateEditor.Api.Api exposing
    ( ServerInfo
    , ToMsg
    , authorizationHeaders
    , authorizedUrl
    , expectMetadata
    , httpFetch
    , httpGet
    , httpPost
    , httpPut
    , jwtDelete
    , jwtFetch
    , jwtFetchEmpty
    , jwtFetchPut
    , jwtGet
    , jwtOrHttpFetch
    , jwtOrHttpGet
    , jwtOrHttpHead
    , jwtOrHttpPut
    , jwtPost
    , jwtPostEmpty
    , jwtPostFile
    , jwtPostFileWithData
    , jwtPostString
    , jwtPut
    , wsUrl
    )

import File exposing (File)
import Http exposing (Error(..))
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Maybe.Extra as Maybe


type alias ToMsg a msg =
    Result Http.Error a -> msg


type alias ServerInfo =
    { apiUrl : String
    , token : Maybe String
    }


jwtOrHttpGet : String -> Decoder a -> ServerInfo -> ToMsg a msg -> Cmd msg
jwtOrHttpGet url decoder serverInfo =
    jwtOrHttp serverInfo jwtGet httpGet url decoder serverInfo


jwtOrHttpPut : String -> E.Value -> ServerInfo -> ToMsg () msg -> Cmd msg
jwtOrHttpPut url body serverInfo =
    jwtOrHttp serverInfo jwtPut httpPut url body serverInfo


jwtOrHttpFetch : String -> Decoder a -> E.Value -> ServerInfo -> ToMsg a msg -> Cmd msg
jwtOrHttpFetch url decoder body serverInfo =
    jwtOrHttp serverInfo jwtFetch httpFetch url decoder body serverInfo


jwtOrHttpHead : String -> ServerInfo -> ToMsg Http.Metadata msg -> Cmd msg
jwtOrHttpHead url serverInfo toMsg =
    Http.request
        { method = "HEAD"
        , headers = authorizationHeaders serverInfo
        , url = serverInfo.apiUrl ++ url
        , body = Http.emptyBody
        , expect = expectMetadata toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


jwtGet : String -> Decoder a -> ServerInfo -> ToMsg a msg -> Cmd msg
jwtGet url decoder serverInfo toMsg =
    createRequest "GET"
        serverInfo
        { url = url
        , expect = expectJson toMsg decoder
        , body = Http.emptyBody
        }


jwtPost : String -> E.Value -> ServerInfo -> ToMsg () msg -> Cmd msg
jwtPost url body serverInfo toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever toMsg
        }


jwtPostString : String -> String -> String -> ServerInfo -> ToMsg () msg -> Cmd msg
jwtPostString url mime contents serverInfo toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.stringBody mime contents
        , expect = Http.expectWhatever toMsg
        }


jwtPostFile : String -> File -> ServerInfo -> ToMsg () msg -> Cmd msg
jwtPostFile url file serverInfo toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.multipartBody [ Http.filePart "file" file ]
        , expect = Http.expectWhatever toMsg
        }


jwtPostFileWithData : String -> List Http.Part -> File -> ServerInfo -> ToMsg () msg -> Cmd msg
jwtPostFileWithData url data file serverInfo toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.multipartBody (Http.filePart "file" file :: data)
        , expect = Http.expectWhatever toMsg
        }


jwtPostEmpty : String -> ServerInfo -> ToMsg () msg -> Cmd msg
jwtPostEmpty url serverInfo toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        }


jwtFetch : String -> Decoder a -> E.Value -> ServerInfo -> ToMsg a msg -> Cmd msg
jwtFetch url decoder body serverInfo toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


jwtFetchEmpty : String -> Decoder a -> ServerInfo -> ToMsg a msg -> Cmd msg
jwtFetchEmpty url decoder serverInfo toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.emptyBody
        , expect = expectJson toMsg decoder
        }


jwtPut : String -> E.Value -> ServerInfo -> ToMsg () msg -> Cmd msg
jwtPut url body serverInfo toMsg =
    createRequest "PUT"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever toMsg
        }


jwtFetchPut : String -> Decoder a -> E.Value -> ServerInfo -> ToMsg a msg -> Cmd msg
jwtFetchPut url decoder body serverInfo toMsg =
    createRequest "PUT"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


jwtDelete : String -> ServerInfo -> ToMsg () msg -> Cmd msg
jwtDelete url serverInfo toMsg =
    createRequest "DELETE"
        serverInfo
        { url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        }


httpGet : String -> Decoder a -> ServerInfo -> ToMsg a msg -> Cmd msg
httpGet url decoder serverInfo toMsg =
    Http.get
        { url = serverInfo.apiUrl ++ url
        , expect = expectJson toMsg decoder
        }


httpPost : String -> E.Value -> ServerInfo -> ToMsg () msg -> Cmd msg
httpPost url body serverInfo toMsg =
    Http.post
        { url = serverInfo.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever toMsg
        }


httpFetch : String -> Decoder a -> E.Value -> ServerInfo -> ToMsg a msg -> Cmd msg
httpFetch url decoder body serverInfo toMsg =
    Http.post
        { url = serverInfo.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


httpPut : String -> E.Value -> ServerInfo -> ToMsg () msg -> Cmd msg
httpPut url body serverInfo toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = serverInfo.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


wsUrl : String -> ServerInfo -> String
wsUrl url serverInfo =
    String.replace "http" "ws" <| authorizedUrl url serverInfo


authorizationHeaders : ServerInfo -> List Http.Header
authorizationHeaders serverInfo =
    case serverInfo.token of
        Just token ->
            [ Http.header "Authorization" ("Bearer " ++ token) ]

        Nothing ->
            []


authorizedUrl : String -> ServerInfo -> String
authorizedUrl url serverInfo =
    let
        urlToken =
            case serverInfo.token of
                Just token ->
                    "?" ++ "Authorization=Bearer%20" ++ token

                Nothing ->
                    ""
    in
    serverInfo.apiUrl ++ url ++ urlToken


expectJson : ToMsg a msg -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        resolve <|
            \string ->
                Result.mapError D.errorToString (D.decodeString decoder string)


createRequest : String -> ServerInfo -> { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
createRequest method { token, apiUrl } { url, body, expect } =
    let
        options =
            { method = method
            , headers = [ Http.header "Authorization" ("Bearer " ++ Maybe.withDefault "" token) ]
            , url = apiUrl ++ url
            , body = body
            , expect = expect
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Http.request options



--expectWhatever : ToMsg () msg -> Http.Expect msg
--expectWhatever toMsg =
--    Http.expectStringResponse toMsg <|
--        resolve <|
--            \_ -> Ok ()
--
--


expectMetadata : ToMsg Http.Metadata msg -> Http.Expect msg
expectMetadata toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err (BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata _ ->
                    Ok metadata


resolve : (String -> Result String a) -> Http.Response String -> Result Http.Error a
resolve toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            Result.mapError BadBody (toResult body)


jwtOrHttp : ServerInfo -> a -> a -> a
jwtOrHttp serverInfo jwtMethod httpMethod =
    if Maybe.isJust serverInfo.token then
        jwtMethod

    else
        httpMethod
