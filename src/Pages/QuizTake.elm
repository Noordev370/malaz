module Pages.QuizTake exposing (init, main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, for, id, name, type_)
import Html.Events as Events
import Json.Decode as D exposing (Decoder, field)


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
    | DecodeQuiz
    | ChoiceSelected


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DecodeQuiz ->
            ( model, Cmd.none )

        ChoiceSelected ->
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
    div []
        [ label [] [ text "choose .quiz file" ]
        , br [] []
        , input [ type_ "file", Html.Attributes.accept ".json,.quiz" ] []
        ]


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
        [ h3 [ class "QE-sec", id section.id ] []
        ]


viewQuestion : Question -> Html Msg
viewQuestion q =
    div [ class "question", id q.id ]
        [ h4 [ class "Qtext" ] [ text q.question ]
        , div [ class "Qchoices" ] (List.map (\x -> viewChoice x q) (Dict.values q.choices))
        ]


viewChoice : Choice -> Question -> Html Msg
viewChoice c q =
    div [ class "Qchoice" ]
        [ label [ for c.id ] [ text c.choice ]
        , input
            [ type_ "radio"
            , id c.id
            , name q.id
            , Events.onClick ChoiceSelected
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
    , choices : Dict String Choice
    , rightChoice : String -- String is the id of the Choice
    , chosenChoice : Maybe String -- in case the user didn't select a choice yet
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


choiceDecoder : Decoder Choice
choiceDecoder =
    D.map2 Choice (D.field "id" D.string) (D.field "choice" D.string)


questionDecoder : Decoder Question
questionDecoder =
    -- chosenChoice field is not present in json data and we want it to return Nothing
    D.map5 Question
        (field "id" D.string)
        (field "question" D.string)
        (field "id" (D.dict choiceDecoder))
        (field "rightChoice" D.string)
        (D.maybe (field "chosenChoice" D.string))


sectionDecoder : Decoder Section
sectionDecoder =
    D.map2 Section (field "id" D.string) (field "title" D.string)


quizElementDecoder : Decoder QuizElement
quizElementDecoder =
    D.oneOf
        [ D.map QuestionElement questionDecoder
        , D.map SectionElement sectionDecoder
        ]


quizDecoder : Decoder Quiz
quizDecoder =
    D.map2 Quiz (field "quizTitle" D.string) (field "quizElements" (D.dict quizElementDecoder))
