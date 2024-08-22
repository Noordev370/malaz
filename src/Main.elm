module Main exposing (..)

--

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Pages.FrontPage as FrontPage
import Pages.QuizEditor as QuizEditorPage
import Pages.QuizTake as QuizTakePage
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
        Frontpage
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
            ( { model | url = url }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.page of
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


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : CurrentPage
    , frontModel : FrontPage.Model
    , quizEditorModel : QuizEditorPage.Model
    , quizTakeModel : QuizTakePage.Model
    }


type CurrentPage
    = Frontpage
    | QuizEditorpage
    | QuizTakepage
