module Pages.FrontPage exposing (Model, Msg, initModel, title, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Attributes


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = viewDocument
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


viewDocument : Model -> Browser.Document Msg
viewDocument _ =
    { title = "Welcome To Malaz", body = [ view ] }


title =
    "Welcome To Malaz"


view : Html Msg
view =
    div [] [ viewHeader, viewMain, viewFooter ]


viewHeader : Html Msg
viewHeader =
    header [] [ h1 [] [ text "Welcome" ] ]


viewMain : Html Msg
viewMain =
    main_ []
        [ a [ Attributes.href "/editor" ] [ text "make a quiz" ]
        , br [] []
        , a [ Attributes.href "/take" ] [ text "take a quiz" ]
        ]


viewFooter : Html Msg
viewFooter =
    footer [] [ h3 [] [ text "Malaz" ], text "by Noor Eldeen" ]


type Model
    = Nothing
