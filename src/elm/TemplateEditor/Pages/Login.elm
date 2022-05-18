module TemplateEditor.Pages.Login exposing
    ( Model
    , Msg(..)
    , UpdateConfig
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Browser exposing (Document)
import Html exposing (Html, button, div, form, h4, i, img, input, label, p, span, text)
import Html.Attributes exposing (alt, class, for, id, placeholder, src, style, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Extra exposing (emptyNode)
import Http
import TemplateEditor.Api.DSW.Tokens as Tokens
import TemplateEditor.Data.AppState exposing (AppState)
import TemplateEditor.Data.Token exposing (Token)
import TemplateEditor.Layouts.DefaultLayout as DefaultLayout
import TemplateEditor.Layouts.PublicLayout as PublicLayout


type alias Model =
    { email : String
    , password : String
    , loggingIn : ActionResult String
    }


init : Model
init =
    { email = ""
    , password = ""
    , loggingIn = ActionResult.Unset
    }


type Msg
    = EmailInput String
    | PasswordInput String
    | Login
    | LoginComplete (Result Http.Error Token)
    | GetProfileInfoFailed (ActionResult String)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , tokenCmd : Token -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        EmailInput email ->
            ( { model | email = email }, Cmd.none )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            ( { model | loggingIn = ActionResult.Loading }
            , Tokens.fetchToken model.email model.password appState (cfg.wrapMsg << LoginComplete)
            )

        LoginComplete result ->
            case result of
                Ok token ->
                    ( model, cfg.tokenCmd token )

                Err error ->
                    ( { model | loggingIn = ActionResult.Error "Login failed" }, Cmd.none )

        GetProfileInfoFailed error ->
            ( { model | loggingIn = error }, Cmd.none )


view : Model -> Document Msg
view model =
    let
        errorBox =
            case model.loggingIn of
                ActionResult.Error error ->
                    div [ class "alert alert-danger" ] [ text error ]

                _ ->
                    emptyNode
    in
    PublicLayout.view (Just "Login") <|
        div [ class "vh-100 pt-5 datenzee-gradient" ]
            [ div [ style "width" "30rem", class "mx-auto" ]
                [ div [ class "card text-black" ]
                    [ div [ class "card-body p-md-5 mx-md-4" ]
                        [ div [ class "text-center" ]
                            [ img [ src "/assets/logo.png", alt "Logo", style "width" "6rem" ] []
                            , h4 [ class "mt-2 mb-5 pb-1" ] [ text "Datenzee Template Editor" ]
                            ]
                        , form [ onSubmit Login ]
                            [ p [] [ text "Please login to your account" ]
                            , errorBox
                            , div [ class "form-group form-group-icon mb-2" ]
                                [ span [ class "input-icon" ] [ i [ class "fas fa-envelope" ] [] ]
                                , input
                                    [ type_ "email"
                                    , id "email"
                                    , placeholder "Email"
                                    , class "form-control"
                                    , onInput EmailInput
                                    ]
                                    []
                                ]
                            , div [ class "form-group form-group-icon mb-4" ]
                                [ span [ class "input-icon" ] [ i [ class "fas fa-key" ] [] ]
                                , input
                                    [ type_ "password"
                                    , id "password"
                                    , placeholder "Password"
                                    , class "form-control"
                                    , onInput PasswordInput
                                    ]
                                    []
                                ]
                            , div [ class "text-center pt-1 pb-1" ]
                                [ button [ class "btn btn-primary btn-block" ] [ text "Log in" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
