module TemplateEditor exposing (main)

import ActionResult
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (div, text)
import Http
import Json.Decode as D
import Task
import Task.Extra as Task
import TemplateEditor.Api.DSW.Data.Token exposing (Token)
import TemplateEditor.Api.DSW.Data.User exposing (User)
import TemplateEditor.Api.DSW.Users as Users
import TemplateEditor.Common.Setters exposing (setDashboard, setLogin, setTemplateEditor)
import TemplateEditor.Data.AppState as AppState exposing (AppState)
import TemplateEditor.Data.Session as Session exposing (Session)
import TemplateEditor.Data.UserInfo as UserInfo exposing (UserInfo)
import TemplateEditor.Layouts.DefaultLayout as DefaultLayout
import TemplateEditor.Layouts.PublicLayout as PublicLayout
import TemplateEditor.Pages.Dashboard as Dashboard
import TemplateEditor.Pages.Login as Login
import TemplateEditor.Pages.TemplateEditor as TemplateEditor
import TemplateEditor.Ports as Ports
import TemplateEditor.Routes as Routes
import TemplateEditor.Routing as Routing
import Time
import Url exposing (Url)


type alias Model =
    { appState : AppState
    , pages : PagesModel
    }


type alias PagesModel =
    { dashboard : Dashboard.Model
    , login : Login.Model
    , templateEditor : TemplateEditor.Model
    }


updateSession : (a -> Session -> Session) -> a -> Model -> Model
updateSession setValue value model =
    let
        appState =
            model.appState
    in
    { model | appState = { appState | session = setValue value model.appState.session } }


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
                , templateEditor = TemplateEditor.initialModel 0
                }
            }

        cmd =
            if appState.invalidSession then
                Ports.clearSessionAndReload ()

            else
                Cmd.batch
                    [ decideInitialRoute model location route ]

        timeCmd =
            Task.perform OnTime Time.now
    in
    ( model
    , Cmd.batch [ cmd, timeCmd ]
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
    | AuthGotToken Token
    | AuthGetCurrentUserComplete (Result Http.Error User)
    | AuthLogout
    | PageDashboardMsg Dashboard.Msg
    | PageLoginMsg Login.Msg
    | PageTemplateEditorMsg TemplateEditor.Msg
    | OnTime Time.Posix


initPage : Model -> ( Model, Cmd Msg )
initPage model =
    let
        pages =
            model.pages
    in
    case model.appState.route of
        Routes.Dashboard ->
            let
                ( dashboardModel, dashboardCmd ) =
                    Dashboard.init model.appState
            in
            ( { model | pages = { pages | dashboard = dashboardModel } }
            , Cmd.map PageDashboardMsg dashboardCmd
            )

        Routes.Login ->
            let
                ( loginModel, loginCmd ) =
                    Login.init
            in
            ( { model | pages = { pages | login = loginModel } }
            , Cmd.map PageLoginMsg loginCmd
            )

        Routes.TemplateEditor id ->
            let
                ( templateEditorModel, templateEditorCmd ) =
                    TemplateEditor.init model.appState id
            in
            ( { model | pages = { pages | templateEditor = templateEditorModel } }
            , Cmd.map PageTemplateEditorMsg templateEditorCmd
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

        AuthGotToken token ->
            let
                newModel =
                    updateSession Session.setToken token model
            in
            ( newModel
            , Users.getCurrentUser newModel.appState AuthGetCurrentUserComplete
            )

        AuthGetCurrentUserComplete result ->
            case result of
                Ok user ->
                    let
                        newModel =
                            updateSession Session.setUser (UserInfo.fromUser user) model
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
                            PageLoginMsg (Login.GetProfileInfoFailed (ActionResult.Error "Unable to get user profile"))
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

        PageDashboardMsg dashboardMsg ->
            let
                ( dashboardModel, dashboardPageCmd ) =
                    Dashboard.update dashboardMsg model.pages.dashboard
            in
            ( { model | pages = setDashboard dashboardModel model.pages }
            , Cmd.map PageDashboardMsg dashboardPageCmd
            )

        PageLoginMsg loginMsg ->
            let
                updateConfig =
                    { wrapMsg = PageLoginMsg
                    , tokenCmd = Task.dispatch << AuthGotToken
                    }

                ( loginPageModel, loginPageCmd ) =
                    Login.update updateConfig model.appState loginMsg model.pages.login
            in
            ( { model | pages = setLogin loginPageModel model.pages }
            , loginPageCmd
            )

        PageTemplateEditorMsg templateEditorMsg ->
            let
                appState =
                    model.appState

                ( seed, templateEditorModel, templateEditorPageCmd ) =
                    TemplateEditor.update model.appState templateEditorMsg model.pages.templateEditor
            in
            ( { model
                | pages = setTemplateEditor templateEditorModel model.pages
                , appState = { appState | seed = seed }
              }
            , Cmd.map PageTemplateEditorMsg templateEditorPageCmd
            )

        OnTime time ->
            let
                appState =
                    model.appState
            in
            ( { model | appState = { appState | currentTime = time } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.appState.route of
        Routes.TemplateEditor _ ->
            Sub.map PageTemplateEditorMsg <| TemplateEditor.subscriptions model.pages.templateEditor

        _ ->
            Sub.none


view : Model -> Document Msg
view model =
    let
        defaultLayoutConfig title =
            { title = Just title
            , logoutMsg = AuthLogout
            }
    in
    case model.appState.route of
        Routes.Dashboard ->
            DefaultLayout.view model.appState
                (defaultLayoutConfig "Dashboard")
                (Html.map PageDashboardMsg (Dashboard.view model.pages.dashboard))

        Routes.TemplateEditor _ ->
            let
                title =
                    ActionResult.unwrap "Loading..." .name model.pages.templateEditor.templateEditor
            in
            DefaultLayout.view model.appState
                (defaultLayoutConfig title)
                (Html.map PageTemplateEditorMsg (TemplateEditor.view model.appState model.pages.templateEditor))

        Routes.Login ->
            PublicLayout.view (Just "Login")
                (Html.map PageLoginMsg (Login.view model.pages.login))

        Routes.NotFound ->
            DefaultLayout.view model.appState
                (defaultLayoutConfig "Not Found")
                (div [] [ text "Not Found" ])


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
