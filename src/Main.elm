module Main exposing (..)

--

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Pages.FrontPage as FrontPage
import Pages.QuizEditor as QuizEditor
import Pages.QuizTake as QuizTake
import Url


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
        url
        FrontPage
        FrontPage.initModel
        QuizEditor.initModel
        QuizTake.initModel
    , Cmd.none
    )


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | URLChanged Url.Url


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
            ( { model | url = url }, Cmd.none )


view : Model -> Browser.Document msg
view model =
    { title = "", body = [ div [] [] ] }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , frontModel : FrontPage.Model
    , quizEditorModel : QuizEditor.Model
    , quizTakeModel : QuizTake.Model
    }


type Page
    = FrontPage
    | QuizEditorPage
    | QuizTakePage
