module Pages.QuizEditor exposing (Model, init, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, id, name, type_, value)
import Html.Events as Events
import Json.Encode as Encode


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
    Model "quiz 1" Dict.empty Nothing


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
                    -- use Dict.insert to replace the existing key with new value not Dict.update
                    Dict.insert sec.id sectionElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        DeleteSection sec ->
            let
                updatedQuizElements =
                    Dict.remove sec.id model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        -- questions related
        InsertQuestion ->
            let
                defaultChoices =
                    Dict.fromList [ ( 0, Choice 0 "choice A" ), ( 1, Choice 1 "choice B" ) ]

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
                    Dict.insert q.id updatedQuestion model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        DeleteQuestion q ->
            let
                updatedQuizElement =
                    Dict.remove q.id model.quizElements
            in
            ( { model | quizElements = updatedQuizElement }, Cmd.none )

        -- choices related
        InsertChoice q ->
            {--to update the question with new choice then update the model with that question
           |
           | add the new choice to the question and return the new question
-}
            let
                addedChoice =
                    Choice (getNextChoiceID q) "New Choice"

                updatedQuestion =
                    addChoiceToQuestion addedChoice q

                qToQuizElement =
                    QuestionElement updatedQuestion

                updatedQuizElements =
                    Dict.insert q.id qToQuizElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        ChangeChoice q c str ->
            let
                updatedChoice =
                    { c | choice = str }

                updatedQuestion =
                    { q | choices = Dict.insert c.id updatedChoice q.choices }

                qToQuizElement =
                    QuestionElement updatedQuestion

                updatedQuizElements =
                    Dict.insert q.id qToQuizElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        DeleteChoice q c ->
            let
                updatedQuestion =
                    { q | choices = Dict.remove c.id q.choices }

                qToQuizElement =
                    QuestionElement updatedQuestion

                updatedQuizElements =
                    Dict.insert q.id qToQuizElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        SetRightChoice q c ->
            let
                updatedQuestion =
                    { q | rightChoice = Just c.id }

                qToQuizElement =
                    QuestionElement (Debug.log "q" updatedQuestion)

                updatedQuizElements =
                    Dict.insert q.id qToQuizElement model.quizElements
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
    main_ [] (List.map (\x -> viewElement x) (Dict.values model.quizElements))


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
        , div [ class "Qchoices" ] (List.map (\x -> viewChoice x q) (Dict.values q.choices))
        , div [ class "one-more-choice" ]
            [ button [ Events.onClick (InsertChoice q) ] [ text "+ choice" ]
            ]
        ]


viewChoice : Choice -> Question -> Html Msg
viewChoice c q =
    div [ class "Qchoice", id (getChoiceIDStr q c) ]
        [ input [ type_ "text", value c.choice, Events.onInput (ChangeChoice q c) ] []
        , input
            [ type_ "radio"
            , name (getQuestionIDStr q)
            , Events.onClick (SetRightChoice q c)
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
    , choices : Dict Int Choice -- Int is the id of the Choice
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


{-| lastIndex: to keep track of the last quiz element index
to incrementt it when adding elements and to access the element by index.
-}
type alias Model =
    { quizTitle : String
    , quizElements : Dict Int QuizElement -- Int is the id of the Question or Section
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
            Dict.insert s.id addedSecElement oldQuizElements
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
            Dict.insert c.id c q.choices
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
            Dict.insert q.id addedQuestionElement oldQuizElements
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


sectionEncoder : Section -> Encode.Value
sectionEncoder s =
    Encode.object
        [ ( "id", Encode.int s.id )
        , ( "title", Encode.string s.title )
        ]



{--questionEncoder : Question -> Encode.Value
questionEncoder q =
    Encode.object
        [ ( "id", Encode.int q.id )
        , ( "question", Encode.string q.question )
        , ("choices",)
        , ("rightChoice" , Encode.int q.rightChoice)
        ]
--}
