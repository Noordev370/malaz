module Pages.QuizTake exposing (init, main)

import Browser
import Dict exposing (Dict)
import File
import File.Select
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, for, id, name, type_)
import Html.Events as Events
import Json.Decode as D exposing (Decoder, field)
import Task


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
    BeforeQuizLoading


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


type Msg
    = NoOp
    | QuizFileRequested
    | QuizFileSelected File.File
    | QuizFileLoaded String
    | ChoiceSelected Question Choice
    | QuizSubmitted Quiz


update : Msg -> Model -> ( Model, Cmd Msg )



{- TODO: refactor this update to pattern match against model first then msg
   instead of msg firs then model. it will make the code much simple and that
   is the logical behaviour in our case
-}


update msg model =
    case msg of
        NoOp ->
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
                BeforeQuizLoading ->
                    ( model, Cmd.none )

                AfterQuizSubmitted _ ->
                    ( model, Cmd.none )

                QuizLoadingFailed _ ->
                    ( model, Cmd.none )

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

        QuizSubmitted quiz ->
            ( AfterQuizSubmitted quiz, Cmd.none )



-- view


view : Model -> Browser.Document Msg
view model =
    case model of
        AfterQuizLoading quiz ->
            { title = quiz.quizTitle, body = viewQuiz quiz }

        BeforeQuizLoading ->
            { title = "Selct a quiz file", body = [ viewQuizSelector ] }

        QuizLoadingFailed error ->
            { title = "Selct a quiz file", body = [ div [] [ text <| D.errorToString error ] ] }

        AfterQuizSubmitted quiz ->
            { title = quiz.quizTitle ++ "result", body = viewQuizSubmittingResult quiz }



-- BeforeLoadingQuiz view


viewQuizSelector : Html Msg
viewQuizSelector =
    div []
        [ label [] [ text "choose .quiz file" ]
        , br [] []
        , button [ type_ "file", Attributes.accept ".quiz", Events.onClick QuizFileRequested ]
            [ text "select" ]
        ]



-- QuizLoaded view


viewQuiz : Quiz -> List (Html Msg)
viewQuiz quiz =
    [ viewHeader quiz, viewMain quiz, submitButton quiz, viewFooter ]


viewHeader : Quiz -> Html Msg
viewHeader quiz =
    header []
        [ h1 [] [ text quiz.quizTitle ] ]


viewMain : Quiz -> Html Msg
viewMain quiz =
    main_ [] (List.map (\x -> viewElement x) (Dict.values quiz.quizElements))


submitButton : Quiz -> Html Msg
submitButton quiz =
    button [ Events.onClick (QuizSubmitted quiz) ] [ text "submit" ]


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
viewSection s =
    h3 [ class "QE-sec", id s.id ] [ text s.title ]


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
            , Events.onClick (ChoiceSelected q c)
            ]
            []
        ]



-- QuizSubmitted view


viewQuizSubmittingResult : Quiz -> List (Html Msg)
viewQuizSubmittingResult quiz =
    [ viewPoints quiz ]


viewPoints : Quiz -> Html Msg
viewPoints quiz =
    div [] [ text <| "your score is " ++ String.fromInt (calculatePoints quiz) ]


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
    | QuizLoadingFailed D.Error -- i don't json decoding errors expect to happen though
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
