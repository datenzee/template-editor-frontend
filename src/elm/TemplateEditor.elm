module TemplateEditor exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (div, h1, i, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (Value)
import Url exposing (Url)


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest


type alias Model =
    {}


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view _ =
    { title = "Template Editor"
    , body =
        [ div [ class "jumbotron m-3" ]
            [ h1 []
                [ i [ class "fas fa-pen-fancy mr-3" ] []
                , text "Template Editor"
                ]
            ]
        ]
    }


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
