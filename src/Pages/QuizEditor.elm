module Pages.QuizEditor exposing (Model, init, update, view)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id, name, type_, value)
import Html.Events as Events
import Json.Encode as Encode
import List.Extra


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
    | ChangeQuestion Question String -- modify question text
    | DeleteQuestion Question
    | InsertSection
    | ChangeSection Section String -- modify section title
    | DeleteSection Section
    | InsertChoice Question
    | ChangeChoice Question Choice String
    | DeleteChoice Question Choice
    | SetRightChoice Question Choice
    | SaveToFile
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
                sectionElement =
                    SectionElement { sec | title = str }

                updatedQuizElements =
                    Array.set sec.id sectionElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        DeleteSection sec ->
            -- deleting will corrupt the indexes so convert it to DeletedElement instead
            let
                updatedQuizElements =
                    Array.set sec.id DeletedElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

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

        ChangeQuestion q str ->
            let
                updatedQuestion =
                    QuestionElement { q | question = str }

                updatedQuizElements =
                    Array.set q.id updatedQuestion model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        DeleteQuestion q ->
            let
                updatedQuizElement =
                    Array.set q.id DeletedElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElement }, Cmd.none )

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
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        ChangeChoice q c str ->
            let
                predicate : Choice -> Bool
                predicate i =
                    i.id == c.id

                updateFunc : Choice -> Choice
                updateFunc choice =
                    { choice | choice = str }

                updatedQuestion =
                    { q | choices = List.Extra.updateIf predicate updateFunc q.choices }

                qToQuizElement =
                    QuestionElement updatedQuestion

                updatedQuizElements =
                    Array.set q.id qToQuizElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        DeleteChoice q c ->
            let
                predicate : Choice -> Bool
                predicate i =
                    i.id == c.id

                updatedQuestion =
                    { q | choices = List.Extra.filterNot predicate q.choices }

                qToQuizElement =
                    QuestionElement updatedQuestion

                updatedQuizElements =
                    Array.set q.id qToQuizElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        SetRightChoice q c ->
            let
                updatedQuestion =
                    { q | rightChoice = Just c.id }

                qToQuizElement =
                    QuestionElement updatedQuestion

                updatedQuizElements =
                    Array.set q.id qToQuizElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        -- others
        ChangeTheme ->
            ( model, Cmd.none )

        SaveToFile ->
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
    div [ class "toolbar" ]
        [ viewQuestionCreationButton
        , viewSectionCreationButton
        , viewThemeChangeButton
        , viewSaveButton
        ]


viewSectionCreationButton : Html Msg
viewSectionCreationButton =
    button [ Events.onClick InsertSection ] [ text "i section" ]


viewQuestionCreationButton : Html Msg
viewQuestionCreationButton =
    button [ Events.onClick InsertQuestion ] [ text "i question" ]


viewThemeChangeButton : Html Msg
viewThemeChangeButton =
    button [ Events.onClick ChangeTheme ] [ text "change theme" ]


viewSaveButton : Html Msg
viewSaveButton =
    button [ Events.onClick SaveToFile ] [ text "Save" ]


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

        DeletedElement ->
            viewDeleted


viewSection : Section -> Html Msg
viewSection section =
    div []
        [ input
            [ type_ "text"
            , value section.title
            , class "QE-sec"
            , id (getSectionIDStr section)
            , Events.onInput (ChangeSection section)
            ]
            []
        , button [ Events.onClick (DeleteSection section) ] [ text "X" ]
        ]


viewQuestion : Question -> Html Msg
viewQuestion q =
    div [ class "question", id (getQuestionIDStr q) ]
        [ input
            [ type_ "text"
            , class "Qtext"
            , value q.question
            , Events.onInput (ChangeQuestion q)
            ]
            []
        , button [ Events.onClick (DeleteQuestion q) ] [ text "X" ]
        , div [ class "Qchoices" ] (List.map (\x -> viewChoice x q) q.choices)
        , div [ class "one-more-choice" ]
            [ button [ Events.onClick (InsertChoice q) ] [ text "+ choice" ]
            ]
        ]


viewDeleted : Html Msg
viewDeleted =
    text ""


viewChoice : Choice -> Question -> Html Msg
viewChoice c q =
    div [ class "Qchoice", id (getChoiceIDStr q c) ]
        [ input [ type_ "text", value c.choice, Events.onInput (ChangeChoice q c) ] []
        , input
            [ type_ "radio"
            , name (getQuestionIDStr q)
            , Events.onDoubleClick (SetRightChoice q c)
            ]
            []
        , button [ Events.onClick (DeleteChoice q c) ] [ text "X" ]
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
    , rightChoice : Maybe Int -- id of the choice, maybe nothing in case the user hasn't set the right choice yet.
    , lastChoiceIndex : Int
    }


type alias Section =
    { title : String
    , id : Int
    }


type QuizElement
    = QuestionElement Question
    | SectionElement Section
    | DeletedElement


{-| lastIndex: to keep track of the last quiz element index
to incrementt it when adding elements and to access the element by index.
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



-- Validators


questionFound : QuizElement -> Bool
questionFound x =
    False


choiceFound : Question -> Bool
choiceFound q =
    False


rightChoiceChosen : Question -> Bool
rightChoiceChosen q =
    False



-- Encoder


choiceEncoder : Choice -> Encode.Value
choiceEncoder c =
    Encode.object
        [ ( "id", Encode.int c.id )
        , ( "choice", Encode.string c.choice )
        ]



-- Utils


listPush : a -> List a -> List a
listPush item list =
    let
        toArray =
            Array.fromList list
    in
    Array.toList (Array.push item toArray)
