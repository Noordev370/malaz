module Pages.FrontPage exposing (Model, initModel, main)

import Browser
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events


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


view : Model -> Browser.Document Msg
view model =
    { title = "Welcome To Malaz", body = [ viewHeader, viewMain, viewFooter ] }


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
