module Main exposing (..)

--

import Browser
import Browser.Navigation as Nav
import File.Download exposing (url)
import Html exposing (..)
import Pages.FrontPage as FrontPage
import Pages.QuizEditor as QuizEditorPage
import Pages.QuizTake as QuizTakePage
import Url
import Url.Parser as Parser


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = URLChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    ( Model key
        (getCurrentPage url)
        FrontPage.initModel
        QuizEditorPage.initModel
        QuizTakePage.initModel
    , Cmd.none
    )


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | URLChanged Url.Url
    | Front FrontPage.Msg
    | QuizEditor QuizEditorPage.Msg
    | QuizTake QuizTakePage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        URLChanged url ->
            ( { model | currentPage = getCurrentPage url }, Cmd.none )

        Front subMsg ->
            let
                ( subModel, subCmd ) =
                    FrontPage.update subMsg model.frontModel
            in
            ( { model | frontModel = subModel }, Cmd.map Front subCmd )

        QuizEditor subMsg ->
            let
                ( subModel, subCmd ) =
                    QuizEditorPage.update subMsg model.quizEditorModel
            in
            ( { model | quizEditorModel = subModel }, Cmd.map QuizEditor subCmd )

        QuizTake subMsg ->
            let
                ( subModel, subCmd ) =
                    QuizTakePage.update subMsg model.quizTakeModel
            in
            ( { model | quizTakeModel = subModel }, Cmd.map QuizTake subCmd )


view : Model -> Browser.Document Msg
view model =
    case model.currentPage of
        Frontpage ->
            { title = FrontPage.title
            , body = [ Html.map Front FrontPage.view ]
            }

        QuizEditorpage ->
            { title = QuizEditorPage.title
            , body = [ Html.map QuizEditor (QuizEditorPage.view model.quizEditorModel) ]
            }

        QuizTakepage ->
            { title = QuizTakePage.title model.quizTakeModel
            , body = [ Html.map QuizTake (QuizTakePage.view model.quizTakeModel) ]
            }

        NotFoundpage ->
            { title = "Not Found"
            , body =
                [ h1 [] [ text "The page you requested is not found" ]
                ]
            }



-- types


type alias Model =
    { key : Nav.Key
    , currentPage : CurrentPage
    , frontModel : FrontPage.Model
    , quizEditorModel : QuizEditorPage.Model
    , quizTakeModel : QuizTakePage.Model
    }


type CurrentPage
    = Frontpage
    | QuizEditorpage
    | QuizTakepage
    | NotFoundpage



-- router


type Route
    = IndexRoute
    | EditorRoute
    | TakeRoute
    | NotFoundRoute


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map IndexRoute Parser.top
        , Parser.map EditorRoute (Parser.s "editor")
        , Parser.map TakeRoute (Parser.s "take")
        ]


myUrlParser : Url.Url -> Route
myUrlParser url =
    Maybe.withDefault NotFoundRoute (Parser.parse routeParser url)


getCurrentPage : Url.Url -> CurrentPage
getCurrentPage url =
    case myUrlParser url of
        NotFoundRoute ->
            NotFoundpage

        EditorRoute ->
            QuizEditorpage

        TakeRoute ->
            QuizTakepage

        IndexRoute ->
            Frontpage
