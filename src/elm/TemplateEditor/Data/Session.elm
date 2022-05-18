module TemplateEditor.Data.Session exposing (Session, decoder, encode, exists, init, setToken, setUser)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import TemplateEditor.Data.Token as Token exposing (Token)
import TemplateEditor.Data.UserInfo as UserInfo exposing (UserInfo)


type alias Session =
    { token : Token
    , user : Maybe UserInfo
    }


init : Session
init =
    { token = Token.empty
    , user = Nothing
    }


decoder : Decoder Session
decoder =
    D.succeed Session
        |> D.required "token" Token.decoder
        |> D.required "user" (D.nullable UserInfo.decoder)


encode : Session -> E.Value
encode session =
    E.object
        [ ( "token", Token.encode session.token )
        , ( "user", E.maybe UserInfo.encode session.user )
        ]


exists : Session -> Bool
exists session =
    session.token.token /= ""


setToken : Token -> Session -> Session
setToken token session =
    { session | token = token }


setUser : UserInfo -> Session -> Session
setUser user session =
    { session | user = Just user }
