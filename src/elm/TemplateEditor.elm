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
import TemplateEditor.Layouts.PublicLayout as PublicLayout
import TemplateEditor.Pages.Dashboard as Dashboard
import TemplateEditor.Pages.Login as Login
import TemplateEditor.Ports as Ports
import TemplateEditor.Routes as Routes
import TemplateEditor.Routing as Routing
import Url exposing (Url)


type alias Model =
    { appState : AppState
    , pages :
        { dashboard : Dashboard.Model
        , login : Login.Model
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
                { dashboard = Dashboard.initialModel
                , login = Login.initialModel
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
    | DashboardPageMsg Dashboard.Msg
    | LoginPageMsg Login.Msg
    | AuthGotToken Token
    | AuthGetCurrentUserComplete (Result Http.Error User)
    | AuthLogout


initPage : Model -> ( Model, Cmd Msg )
initPage model =
    let
        pages =
            model.pages
    in
    case model.appState.route of
        Routes.Login ->
            let
                ( loginModel, loginCmd ) =
                    Login.init
            in
            ( { model | pages = { pages | login = loginModel } }
            , Cmd.map LoginPageMsg loginCmd
            )

        Routes.Dashboard ->
            let
                ( dashboardModel, dashboardCmd ) =
                    Dashboard.init model.appState
            in
            ( { model | pages = { pages | dashboard = dashboardModel } }
            , Cmd.map DashboardPageMsg dashboardCmd
            )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange location ->
            let
                newRoute =
                    Routing.parseLocation location

                ( newModel, cmd ) =
                    initPage <|
                        setRoute newRoute model
            in
            ( newModel, cmd )

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

        DashboardPageMsg dashboardMsg ->
            let
                pages =
                    model.pages

                ( dashboardModel, dashboardPageCmd ) =
                    Dashboard.update dashboardMsg pages.dashboard
            in
            ( { model | pages = { pages | dashboard = dashboardModel } }
            , Cmd.map DashboardPageMsg dashboardPageCmd
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
                        [ Ports.storeSession (Session.encode newModel.appState.session)
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
            DefaultLayout.view model.appState
                { title = Just "Dashboard"
                , logoutMsg = AuthLogout
                }
                (Html.map DashboardPageMsg (Dashboard.view model.pages.dashboard))

        Routes.Login ->
            PublicLayout.view (Just "Login")
                (Html.map LoginPageMsg (Login.view model.pages.login))

        Routes.NotFound ->
            DefaultLayout.view model.appState
                { title = Just "Not Found"
                , logoutMsg = AuthLogout
                }
            <|
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
