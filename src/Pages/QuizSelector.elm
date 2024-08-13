module Pages.QuizSelector exposing (Msg(..), init, main)

import Browser
import Html exposing (..)
import Html.Attributes


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initModel : Model
initModel =
    Nothing


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document msg
view model =
    { title = "", body = [ div [] [] ] }


type Model
    = Nothing
