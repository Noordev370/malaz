module Pages.QuizEditor exposing (Model, init, update, view)

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


initModel : Model
initModel =
    Model "quiz 1" Array.empty Nothing


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


type Msg
    = InsertQuestion
    | ChangeQuestion -- modify question text
    | DeleteQuestion
    | InsertSection
    | ChangeSection Section String -- modify question text
    | DeleteSection
    | InsertChoice Question
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

        ChangeSection sec str ->
            let
                updatedSection =
                    { sec | title = String.toUpper str }

                secToQuizElement =
                    SectionElement updatedSection

                updatedQuizElement =
                    Array.set sec.id secToQuizElement model.quizElements

                updatedModel =
                    { model | quizElements = updatedQuizElement }
            in
            ( updatedModel, Cmd.none )

        DeleteSection ->
            ( model, Cmd.none )

        -- questions related
        InsertQuestion ->
            let
                defaultChoices =
                    [ Choice 0 "choice A", Choice 1 "choice B" ]

                addedQuestion =
                    Question (getNextQuestionID model) "New Question ....." defaultChoices (Just 0) 1

                updatedModel =
                    addQuestionToQuiz addedQuestion model
            in
            ( updatedModel, Cmd.none )

        ChangeQuestion ->
            ( model, Cmd.none )

        DeleteQuestion ->
            ( model, Cmd.none )

        -- choices related
        InsertChoice q ->
            {--| to update the question with new choice then update the model with that question
           |
           | add the new choice to the question and return the new question
           | then covert the question to quiz element
           | then set that element by Array.set to be new value instead of the old one.
           |
           | the question id is the same as the index of the corresponding elemnt
-}
            let
                addedChoice =
                    Choice (getNextChoiceID q) "New Choice"

                updatedQuestion =
                    addChoiceToQuestion addedChoice q

                qToQuizElement =
                    QuestionElement updatedQuestion

                updatedQuizElements =
                    Array.set q.id qToQuizElement model.quizElements

                updatedModel =
                    { model | quizElements = updatedQuizElements }
            in
            ( updatedModel, Cmd.none )

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
    button [ Events.onClick ChangeTheme ] [ text "change theme" ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [] [ input [ type_ "textbox", value model.quizTitle ] [] ]


viewMain : Model -> Html Msg
viewMain model =
    main_ [] (List.map (\x -> viewElement x) (Array.toList model.quizElements))


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
    input
        [ type_ "text"
        , value section.title
        , class "QE-sec"
        , id (getSectionIDStr section)
        , Events.onInput (ChangeSection section)
        ]
        []


viewQuestion : Question -> Html Msg
viewQuestion q =
    div [ class "question" ]
        [ div [ class "Qtext", id (getQuestionIDStr q) ] [ text q.question ]
        , div [ class "Qchoices" ] (List.map (\x -> viewChoice x q) q.choices)
        , div [ class "one-more-choice" ]
            [ button [ Events.onClick (InsertChoice q) ] [ text "+ choice" ]
            ]
        ]


viewChoice : Choice -> Question -> Html Msg
viewChoice c q =
    div [ class "Qchoice" ]
        [ label [ for (getChoiceIDStr q c) ] [ text c.choice ]
        , input [ type_ "radio", id (getChoiceIDStr q c), name (getQuestionIDStr q) ] []
        ]



-- Types


type alias Choice =
    { id : Int
    , choice : String
    }


type alias Question =
    { id : Int
    , question : String
    , choices : List Choice
    , correctChoice : Maybe Int -- id of the choice, maybe nothing in case the user hasn't set the right choice yet.
    , lastChoiceIndex : Int
    }


type alias Section =
    { title : String
    , id : Int
    }


type QuizElement
    = QuestionElement Question
    | SectionElement Section


{-| lastIndex: to keep track of the last quiz element index
to increment or decrement it when adding or deleting elements
and to access the element by index
-}
type alias Model =
    { quizTitle : String
    , quizElements : Array QuizElement
    , lastIndex : Maybe Int -- maybe nothing in case ther is no elements, the index starts from 0 .
    }



-- types helpers
-- sections related


getNextSecID : Model -> Int
getNextSecID model =
    case model.lastIndex of
        Nothing ->
            0

        Just int ->
            1 + int


addSectionToQuiz : Section -> Model -> Model
addSectionToQuiz s model =
    let
        oldQuizElements =
            model.quizElements

        addedSecElement =
            SectionElement s

        updatedQuizElements =
            Array.push addedSecElement oldQuizElements
    in
    { model | quizElements = updatedQuizElements, lastIndex = Just s.id }


getSectionIDStr : Section -> String
getSectionIDStr s =
    "e" ++ String.fromInt s.id



-- choices related


getNextChoiceID : Question -> Int
getNextChoiceID q =
    q.lastChoiceIndex + 1


addChoiceToQuestion : Choice -> Question -> Question
addChoiceToQuestion c q =
    let
        updatesChoices =
            listPush c q.choices
    in
    { q | choices = updatesChoices, lastChoiceIndex = c.id }


getChoiceIDStr : Question -> Choice -> String
getChoiceIDStr q c =
    getQuestionIDStr q ++ "c" ++ String.fromInt c.id



-- questions related


getNextQuestionID : Model -> Int
getNextQuestionID model =
    case model.lastIndex of
        Nothing ->
            0

        Just int ->
            1 + int


addQuestionToQuiz : Question -> Model -> Model
addQuestionToQuiz q model =
    let
        oldQuizElements =
            model.quizElements

        addedQuestionElement =
            QuestionElement q

        updatedQuizElements =
            Array.push addedQuestionElement oldQuizElements
    in
    { model | quizElements = updatedQuizElements, lastIndex = Just q.id }


getQuestionIDStr : Question -> String
getQuestionIDStr q =
    "e" ++ String.fromInt q.id



-- utils


listPush : a -> List a -> List a
listPush item list =
    let
        toArray =
            Array.fromList list
    in
    Array.toList (Array.push item toArray)
