module Pages.QuizEditor exposing (Model, init, initModel, update, view)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, for, id, name, type_, value)
import Html.Events as Events


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- create initial model


initQuestion : Question
initQuestion =
    Question 0 "New Question ..." initChoices "q0c0" Nothing 1


initChoices : Array Choice
initChoices =
    Array.fromList [ Choice 0 "choice A", Choice 1 "choice B" ]


initModel : Model
initModel =
    Model "quiz 1" (Array.fromList [ QuestionElement initQuestion ]) 0 Nothing


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


type Msg
    = InsertQuestion
    | ChangeQuestion -- modify question text
    | DeleteQuestion
    | InsertSection
    | ChangeSection
    | DeleteSection
    | InsertChoice
    | ChangeChoice
    | DeleteChoice
    | ChangeTheme


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --  sections related
        InsertSection ->
            let
                addedSection =
                    Section "New Section" (getNextSecID model)

                updatedModel =
                    addSectionToQuiz addedSection model
            in
            ( updatedModel, Cmd.none )

        ChangeSection ->
            ( model, Cmd.none )

        DeleteSection ->
            ( model, Cmd.none )

        -- questions related
        InsertQuestion ->
            let
                addedQuestion =
                    Question (getNextQuestionID model) "......." initChoices "bla" Nothing 1

                updatedModel =
                    addQuestionToQuiz addedQuestion model
            in
            ( updatedModel, Cmd.none )

        ChangeQuestion ->
            ( model, Cmd.none )

        DeleteQuestion ->
            ( model, Cmd.none )

        -- choices related
        InsertChoice ->
            ( model, Cmd.none )

        ChangeChoice ->
            ( model, Cmd.none )

        DeleteChoice ->
            ( model, Cmd.none )

        -- others
        ChangeTheme ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Malaz Quiz Editor"
    , body = [ viewToolbar, viewHeader model, viewMain model, viewFooter ]
    }


viewToolbar : Html Msg
viewToolbar =
    div [ class "toolbar" ] [ viewQuestionCreationButton, viewSectionCreationButton, viewThemeChangeButton ]


viewSectionCreationButton : Html Msg
viewSectionCreationButton =
    button [ Events.onClick InsertSection ] [ text "i section" ]


viewQuestionCreationButton : Html Msg
viewQuestionCreationButton =
    button [ Events.onClick InsertQuestion ] [ text "i question" ]


viewThemeChangeButton : Html Msg
viewThemeChangeButton =
    button [] [ text "change theme" ]


viewHeader : Model -> Html Msg
viewHeader quiz =
    header [] [ input [ type_ "textbox", value quiz.quizTitle ] [] ]


viewMain : Model -> Html Msg
viewMain quiz =
    main_ [] (List.map (\x -> viewElement x) (Array.toList quiz.quizElements))


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
    input [ type_ "text", value section.sectionTitle, class "QE-sec", id (getSectionIDStr section) ] []


viewQuestion : Question -> Html Msg
viewQuestion q =
    div [ class "question" ]
        [ div [ class "Qtext", id (getQuestionIDStr q) ] [ text q.question ]
        , div [ class "Qchoices" ] (Array.toList <| Array.map (\x -> viewChoice x q) q.choices)
        ]


viewChoice : Choice -> Question -> Html Msg
viewChoice c q =
    div [ class "Qchoice" ]
        [ label [ for (getChoiceIDStr q c) ] [ text c.choice ]
        , input [ type_ "radio", name (getChoiceIDStr q c) ] []
        ]



-- Types


type alias Choice =
    { choiceID : Int
    , choice : String
    }


type alias Question =
    { questionID : Int
    , question : String
    , choices : Array Choice
    , correctChoice : String -- id of the choice.
    , chosenChoice : Maybe String -- id of the choice, will be encoded to null
    , currentChoiceID : Int
    }


type alias Section =
    { sectionTitle : String
    , sectionID : Int
    }


type QuizElement
    = QuestionElement Question
    | SectionElement Section


{-| currentID: to keep track of the last question id
to increment or decrement it when adding or deleting questions
and will not be encoded to json.
-}
type alias Model =
    { quizTitle : String
    , quizElements : Array QuizElement
    , currentQuestionID : Int
    , currentSectionID : Maybe Int -- maybe nothing in case ther is no sections, the index starts from 0
    }



-- types helpers
-- sections related


getNextSecID : Model -> Int
getNextSecID quiz =
    case quiz.currentSectionID of
        Nothing ->
            0

        Just int ->
            1 + int


addSectionToQuiz : Section -> Model -> Model
addSectionToQuiz sec quiz =
    let
        oldQuizElements =
            quiz.quizElements

        addedSecElement =
            SectionElement sec

        updatedQuizElements =
            Array.push addedSecElement oldQuizElements
    in
    { quiz | quizElements = updatedQuizElements, currentSectionID = Just sec.sectionID }


getSectionIDStr : Section -> String
getSectionIDStr section =
    "s" ++ String.fromInt section.sectionID



-- choices related


getNextChoiceID : Question -> Int
getNextChoiceID question =
    question.currentChoiceID + 1


addChoiceToQuestion : Choice -> Question -> Question
addChoiceToQuestion choice question =
    let
        updatesChoices =
            Array.push choice question.choices
    in
    { question | choices = updatesChoices, currentChoiceID = choice.choiceID }


getChoiceIDStr : Question -> Choice -> String
getChoiceIDStr q c =
    getQuestionIDStr q ++ "c" ++ String.fromInt c.choiceID



-- questions related


getNextQuestionID : Model -> Int
getNextQuestionID m =
    m.currentQuestionID + 1


addQuestionToQuiz : Question -> Model -> Model
addQuestionToQuiz question quiz =
    let
        oldQuizElements =
            quiz.quizElements

        addedQuestionElement =
            QuestionElement question

        updatedQuizElements =
            Array.push addedQuestionElement oldQuizElements
    in
    { quiz | quizElements = updatedQuizElements, currentQuestionID = question.questionID }


getQuestionIDStr : Question -> String
getQuestionIDStr q =
    "q" ++ String.fromInt q.questionID
