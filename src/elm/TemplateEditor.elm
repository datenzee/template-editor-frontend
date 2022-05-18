module TemplateEditor exposing (main)

import ActionResult
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (a, div, text)
import Html.Attributes exposing (class)
import Html.Events.Extra exposing (onLinkClick)
import Http
import Json.Decode as D
import Task.Extra as Task
import TemplateEditor.Api.DSW.Users as Users
import TemplateEditor.Data.AppState as AppState exposing (AppState)
import TemplateEditor.Data.Session as Session exposing (Session)
import TemplateEditor.Data.Token exposing (Token)
import TemplateEditor.Data.User as User exposing (User)
import TemplateEditor.Data.UserInfo exposing (UserInfo)
import TemplateEditor.Layouts.DefaultLayout as DefaultLayout
import TemplateEditor.Pages.Login as Login
import TemplateEditor.Ports as Ports
import TemplateEditor.Routes as Routes
import TemplateEditor.Routing as Routing
import Url exposing (Url)


type alias Model =
    { appState : AppState
    , pages :
        { login : Login.Model
        }
    }


setToken : Token -> Model -> Model
setToken token model =
    setSession (Session.setToken token model.appState.session) model


setUser : UserInfo -> Model -> Model
setUser userInfo model =
    setSession (Session.setUser userInfo model.appState.session) model


setSession : Session -> Model -> Model
setSession session model =
    let
        appState =
            model.appState
    in
    { model | appState = { appState | session = session } }


init : D.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags location key =
    let
        route =
            Routing.parseLocation location

        appState =
            AppState.init flags route key

        model =
            { appState = appState
            , pages =
                { login = Login.init
                }
            }

        cmd =
            if appState.invalidSession then
                Ports.clearSessionAndReload ()

            else
                Cmd.batch
                    [ decideInitialRoute model location route ]
    in
    ( model
    , cmd
    )


decideInitialRoute : Model -> Url -> Routes.Route -> Cmd Msg
decideInitialRoute model location route =
    let
        dispatchUrlChange =
            Task.dispatch (OnUrlChange location)

        userLoggedIn =
            Session.exists model.appState.session
    in
    case route of
        Routes.Login ->
            if userLoggedIn then
                Routing.cmdNavigate model.appState Routes.Dashboard

            else
                dispatchUrlChange

        _ ->
            if userLoggedIn then
                dispatchUrlChange

            else
                Routing.cmdNavigate model.appState Routes.Login


setRoute : Routes.Route -> Model -> Model
setRoute route model =
    let
        appState =
            model.appState
    in
    { model | appState = { appState | route = route } }


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | LoginPageMsg Login.Msg
    | AuthGotToken Token
    | AuthGetCurrentUserComplete (Result Http.Error User)
    | AuthLogout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange location ->
            let
                newRoute =
                    Routing.parseLocation location

                newModel =
                    setRoute newRoute model

                -- TODO
                --|> initLocalModel
            in
            ( newModel, Cmd.none )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.appState.navigationKey (Url.toString url) )

                Browser.External url ->
                    if url == "" then
                        ( model, Cmd.none )

                    else
                        ( model, Navigation.load url )

        LoginPageMsg loginMsg ->
            let
                pages =
                    model.pages

                updateConfig =
                    { wrapMsg = LoginPageMsg
                    , tokenCmd = Task.dispatch << AuthGotToken
                    }

                ( loginPageModel, loginPageCmd ) =
                    Login.update updateConfig model.appState loginMsg pages.login
            in
            ( { model | pages = { pages | login = loginPageModel } }
            , loginPageCmd
            )

        AuthGotToken token ->
            let
                newModel =
                    setToken token model
            in
            ( newModel
            , Users.getCurrentUser newModel.appState AuthGetCurrentUserComplete
            )

        AuthGetCurrentUserComplete result ->
            case result of
                Ok user ->
                    let
                        newModel =
                            setUser (User.toUserInfo user) model
                    in
                    ( newModel
                    , Cmd.batch
                        [ Ports.storeSession (Session.encode model.appState.session)
                        , Routing.cmdNavigate newModel.appState Routes.Dashboard
                        ]
                    )

                Err error ->
                    let
                        failedMsg =
                            LoginPageMsg (Login.GetProfileInfoFailed (ActionResult.Error "Unable to get user profile"))
                    in
                    ( model, Task.dispatch failedMsg )

        AuthLogout ->
            let
                appState =
                    model.appState
            in
            ( { model | appState = { appState | session = Session.init } }
            , Cmd.batch
                [ Ports.clearSession ()
                , Routing.cmdNavigate model.appState Routes.Login
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    case model.appState.route of
        Routes.Dashboard ->
            DefaultLayout.view model.appState Nothing <|
                div []
                    [ text "Dashboard"
                    , a [ onLinkClick AuthLogout, class "link ml-2" ] [ text "Logout" ]
                    ]

        Routes.Login ->
            documentMap LoginPageMsg <|
                Login.view model.pages.login

        Routes.NotFound ->
            DefaultLayout.view model.appState (Just "Not Found") <|
                div [] [ text "Not Found" ]


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--


documentMap : (msg -> Msg) -> Document msg -> Document Msg
documentMap mapBody { title, body } =
    { title = title
    , body = List.map (Html.map mapBody) body
    }
