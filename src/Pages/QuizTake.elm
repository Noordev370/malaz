module Pages.QuizTake exposing (Model, Msg, initModel, title, update, view)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import File
import File.Select
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, for, id, name, type_)
import Html.Events as Events
import Html.Lazy exposing (lazy)
import Json.Decode as D exposing (Decoder, field)
import Task


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
    BeforeQuizLoading


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


type Msg
    = Focus (Result Browser.Dom.Error ())
    | QuizFileRequested
    | QuizFileSelected File.File
    | QuizFileLoaded String
    | ChoiceSelected Question Choice
    | QuizSubmitted Quiz


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focus _ ->
            ( model, Cmd.none )

        QuizFileRequested ->
            ( model, File.Select.file [ "text/json" ] QuizFileSelected )

        QuizFileSelected file ->
            ( model, Task.perform QuizFileLoaded (File.toString file) )

        QuizFileLoaded json ->
            let
                tryDecode =
                    D.decodeString quizDecoder json
            in
            case tryDecode of
                Ok quiz ->
                    ( AfterQuizLoading quiz, Cmd.none )

                Err error ->
                    ( QuizLoadingFailed error, Cmd.none )

        ChoiceSelected q c ->
            case model of
                AfterQuizLoading quiz ->
                    let
                        updatedQuestion =
                            { q | chosenChoice = Just c.id }

                        qToQuizElement =
                            QuestionElement updatedQuestion

                        updatedQuizElements =
                            Dict.insert q.id qToQuizElement quiz.quizElements

                        updatedQuiz =
                            { quiz | quizElements = updatedQuizElements }
                    in
                    ( AfterQuizLoading updatedQuiz, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        QuizSubmitted quiz ->
            {- if all question answered switch to AfterQuizSubmitted model
               if not focus on the first element of unanswered questions
            -}
            let
                qList =
                    extractQuestionsFromElements (Dict.values quiz.quizElements)

                unAnsweredQuestions =
                    getUnAnsweredQuestions qList
            in
            if List.isEmpty unAnsweredQuestions then
                ( AfterQuizSubmitted quiz, Cmd.none )

            else
                ( model
                , case List.head unAnsweredQuestions of
                    Nothing ->
                        Cmd.none

                    Just q ->
                        focusElementByID q.id
                )



-- view


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = title model, body = [ view model ] }


title : Model -> String
title model =
    case model of
        AfterQuizLoading quiz ->
            quiz.quizTitle

        BeforeQuizLoading ->
            "Selct a quiz file"

        QuizLoadingFailed _ ->
            "Selct a quiz file"

        AfterQuizSubmitted quiz ->
            quiz.quizTitle ++ "result"


view : Model -> Html Msg
view model =
    case model of
        AfterQuizLoading quiz ->
            viewQuiz quiz

        BeforeQuizLoading ->
            viewQuizSelector

        QuizLoadingFailed error ->
            div [] [ text <| D.errorToString error ]

        AfterQuizSubmitted quiz ->
            viewQuizSubmittingResult quiz



-- BeforeLoadingQuiz view


viewQuizSelector : Html Msg
viewQuizSelector =
    div [ class "QTS" ]
        [ label [] [ text "choose .quiz file" ]
        , br [] []
        , button [ type_ "file", Attributes.accept ".quiz", Events.onClick QuizFileRequested ]
            [ text "select" ]
        ]



-- QuizLoaded view


viewQuiz : Quiz -> Html Msg
viewQuiz quiz =
    div [ class "QTT center-page" ] [ lazy viewHeader quiz, lazy viewMain quiz, lazy submitButton quiz ]


viewHeader : Quiz -> Html Msg
viewHeader quiz =
    header [ class "center-txt" ]
        [ h1 [] [ text quiz.quizTitle ] ]


viewMain : Quiz -> Html Msg
viewMain quiz =
    main_ [] (List.map (\x -> viewElement x) (Dict.values quiz.quizElements))


submitButton : Quiz -> Html Msg
submitButton quiz =
    button [ Events.onClick (QuizSubmitted quiz) ] [ text "submit" ]


viewElement : QuizElement -> Html Msg
viewElement e =
    case e of
        SectionElement section ->
            lazy viewSection section

        QuestionElement question ->
            lazy viewQuestion question


viewSection : Section -> Html Msg
viewSection s =
    h3 [ class "QTT-section", id s.id ] [ text s.title ]


viewQuestion : Question -> Html Msg
viewQuestion q =
    div [ class "QTT-question", id q.id, Attributes.tabindex 1 ]
        [ h4 [ class "Qtext" ] [ text q.question ]
        , div [ class "Qchoices" ] (List.map (viewChoice q) (Dict.values q.choices))
        ]


viewChoice : Question -> Choice -> Html Msg
viewChoice q c =
    div [ class "Qchoice" ]
        [ label [ for c.id, class "Ctext" ] [ text c.choice ]
        , input
            [ type_ "radio"
            , class "Cradio"
            , id c.id
            , name q.id
            , Events.onClick (ChoiceSelected q c)
            ]
            []
        ]



-- QuizSubmitted view


viewQuizSubmittingResult : Quiz -> Html Msg
viewQuizSubmittingResult quiz =
    viewPoints quiz


viewPoints : Quiz -> Html Msg
viewPoints quiz =
    div [ class "QTR" ] [ text <| "your score is " ++ String.fromInt (calculatePoints quiz) ]


calculatePoints : Quiz -> Int
calculatePoints quiz =
    let
        questionList =
            extractQuestionsFromElements (Dict.values quiz.quizElements)

        pointsList =
            List.map questionToPoint questionList
    in
    List.sum pointsList


questionToPoint : Question -> Int
questionToPoint q =
    {- don't worry the chosenChoice will always be Just String when submitting
       due to the validation we do before submitting
    -}
    if q.rightChoice == Maybe.withDefault "" q.chosenChoice then
        1

    else
        0



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
    { id : String
    , title : String
    }


type QuizElement
    = SectionElement Section
    | QuestionElement Question


type alias Quiz =
    { quizTitle : String
    , quizElements : Dict String QuizElement -- String is the id of the Question or Section
    }


type Model
    = BeforeQuizLoading
    | AfterQuizLoading Quiz
    | QuizLoadingFailed D.Error -- i don't expect json decoding errors to happen though
    | AfterQuizSubmitted Quiz



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
        (field "choices" (D.dict choiceDecoder))
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



-- helpers


extractQuestionsFromElements : List QuizElement -> List Question
extractQuestionsFromElements qeList =
    let
        elementToQuestion : QuizElement -> Maybe Question
        elementToQuestion qe =
            case qe of
                QuestionElement q ->
                    Just q

                _ ->
                    Nothing
    in
    List.filterMap elementToQuestion qeList


focusElementByID : String -> Cmd Msg
focusElementByID el =
    Task.attempt Focus (Browser.Dom.focus el)


getUnAnsweredQuestions : List Question -> List Question
getUnAnsweredQuestions qList =
    let
        isQuestionUnAnswered : Question -> Bool
        isQuestionUnAnswered q =
            q.chosenChoice == Nothing
    in
    List.filter isQuestionUnAnswered qList
