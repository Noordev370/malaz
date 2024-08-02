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


defaultQuestion : Question
defaultQuestion =
    Question "q1" "Question 1 ......" defaultChoices "q1c1" Nothing


defaultChoices : Array Choice
defaultChoices =
    Array.fromList [ Choice "q1c1" "choice A", Choice "q1c2" "choice B" ]


initModel : Model
initModel =
    Model "quiz 1" (Array.fromList [ QuestionElement defaultQuestion ]) 0 Nothing


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


type Msg
    = InsertQuestion
    | InsertSection
    | DeleteQuestion
    | DeleteSection
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

        DeleteSection ->
            ( model, Cmd.none )

        -- questions related
        InsertQuestion ->
            ( model, Cmd.none )

        DeleteQuestion ->
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
    div [] [ viewQuestionCreationButton, viewSectionCreationButton, viewThemeChangeButton ]


viewSectionCreationButton : Html Msg
viewSectionCreationButton =
    button [ Events.onClick InsertSection ] [ text "i section" ]


viewQuestionCreationButton : Html Msg
viewQuestionCreationButton =
    button [] [ text "i question" ]


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
    input [ type_ "text", value section.sectionTitle, id (getSectionIDStr section.sectionID) ] []


viewQuestion : Question -> Html Msg
viewQuestion question =
    div [ class "question" ]
        [ div [ class "Qtext", id "" ] [ text question.question ]
        , div [ class "Qchoices", id "" ] (Array.toList <| Array.map (\x -> viewChoice x question.questionID) question.choices)
        ]


viewChoice : Choice -> String -> Html Msg
viewChoice choice radioName =
    div [ class "Qchoice" ]
        [ label [ for choice.choiceID ] [ text choice.choice ]
        , input [ type_ "radio", name radioName, id choice.choiceID ] []
        ]



-- Types


type alias Choice =
    { choiceID : String
    , choice : String
    }


type alias Question =
    { questionID : String
    , question : String
    , choices : Array Choice
    , correctChoice : String -- id of the choice.
    , chosenChoice : Maybe String -- id of the choice, will be encoded to null
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


getSectionIDStr : Int -> String
getSectionIDStr id =
    "s" ++ String.fromInt id
