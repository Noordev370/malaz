module Pages.QuizTake exposing (init, main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, for, id, name, type_, value)
import Html.Events as Events
import Json.Decode as D


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
    BeforeLoadingQuiz


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



-- view


view : Model -> Browser.Document Msg
view model =
    case model of
        QuizLoaded quiz ->
            { title = quiz.quizTitle, body = [ viewQuiz quiz ] }

        BeforeLoadingQuiz ->
            { title = "Selct a quiz file", body = [ viewQuizSelector ] }


viewQuizSelector : Html Msg
viewQuizSelector =
    div [] [ input [ type_ "file" ] [] ]


viewQuiz : Quiz -> Html Msg
viewQuiz quiz =
    div [] [ viewHeader quiz, viewMain quiz, viewFooter ]


viewHeader : Quiz -> Html Msg
viewHeader quiz =
    header []
        [ h1 [] [ text quiz.quizTitle ] ]


viewMain : Quiz -> Html Msg
viewMain quiz =
    main_ [] (List.map (\x -> viewElement x) (Dict.values quiz.quizElements))


viewFooter : Html Msg
viewFooter =
    footer [] [ h3 [] [ text "Malaz" ], text "by Noor Eldeen" ]


viewElement : QuizElement -> Html Msg
viewElement e =
    case e of
        SectionElement section ->
            viewSection section

        QuestionElement question ->
            viewQuestion question


viewSection : Section -> Html Msg
viewSection section =
    div []
        [ h3
            [ value section.title
            , class "QE-sec"
            , id section.id
            ]
            []
        ]


viewQuestion : Question -> Html Msg
viewQuestion q =
    div [ class "question", id q.id ]
        [ h4
            [ type_ "text"
            , class "Qtext"
            ]
            [ text q.question ]
        , div [ class "Qchoices" ] (List.map (\x -> viewChoice x q) (Dict.values q.choices))
        ]


viewChoice : Choice -> Question -> Html Msg
viewChoice c q =
    div [ class "Qchoice", id c.id ]
        [ label [ for c.id ] [ text c.choice ]
        , input
            [ type_ "radio"
            , name q.id
            , Events.onClick (SetRightChoice q c)
            ]
            []
        ]



-- Types


type alias Choice =
    { id : String
    , choice : String
    }


type alias Question =
    { id : String
    , question : String
    , choices : Dict String Choice -- String is the id of the Choice
    , rightChoice : Int
    , chosenChoice : Maybe Int
    }


type alias Section =
    { title : String
    , id : String
    }


type QuizElement
    = QuestionElement Question
    | SectionElement Section


type alias Quiz =
    { quizTitle : String
    , quizElements : Dict String QuizElement -- String is the id of the Question or Section
    }


type Model
    = BeforeLoadingQuiz
    | QuizLoaded Quiz



-- Decoders
