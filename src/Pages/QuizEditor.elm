module Pages.QuizEditor exposing (Model, Msg, initModel, title, update, view)

import Browser
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (..)
import Html.Attributes exposing (class, id, name, type_, value)
import Html.Events as Events
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Encode as Encode
import Process
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
    Model "New Quiz" Dict.empty Nothing Nothing


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


type Msg
    = ChangeQuizTitle String
    | InsertQuestion
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
    | RemovePopup ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeQuizTitle str ->
            ( { model | quizTitle = str }, Cmd.none )

        --  sections related
        InsertSection ->
            let
                addedSection =
                    Section "New Section ....." (getNextElementID model)

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
                    Question (getNextElementID model) "New Question ....." defaultChoices (Just 0) 1

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
                    QuestionElement updatedQuestion

                updatedQuizElements =
                    Dict.insert q.id qToQuizElement model.quizElements
            in
            ( { model | quizElements = updatedQuizElements }, Cmd.none )

        -- others
        SaveToFile ->
            let
                fileName =
                    model.quizTitle ++ ".json"
            in
            if not (isQuestionFound model) then
                ( { model | popup = Just (Popup "you forgot to add questions") }
                , waitThenRemovePopup
                )

            else if not (isAllQuestionsHaveChoices model) then
                ( { model | popup = Just (Popup "check that all questions have choices") }
                , waitThenRemovePopup
                )

            else
                ( model, Download.string fileName "text/json" (generateJson model) )

        RemovePopup _ ->
            ( { model | popup = Nothing }, Cmd.none )



-- view


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = title
    , body = [ view model ]
    }


title =
    "Malaz Quiz Editor"


view : Model -> Html Msg
view model =
    div [ class "QE" ] [ viewToolbar, lazy viewHeader model, lazy viewMain model, lazy viewPopup model.popup ]


viewToolbar : Html Msg
viewToolbar =
    div [ class "toolbar" ]
        [ viewQuestionCreationButton
        , viewSectionCreationButton
        , viewSaveButton
        ]


viewSectionCreationButton : Html Msg
viewSectionCreationButton =
    button [ Events.onClick InsertSection ] [ text "i section" ]


viewQuestionCreationButton : Html Msg
viewQuestionCreationButton =
    button [ Events.onClick InsertQuestion ] [ text "i question" ]


viewSaveButton : Html Msg
viewSaveButton =
    button [ Events.onClick SaveToFile ] [ text "Save" ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "center-txt" ]
        [ input
            [ type_ "textbox"
            , Events.onInput ChangeQuizTitle
            , value model.quizTitle
            ]
            []
        ]


viewMain : Model -> Html Msg
viewMain model =
    Keyed.node "main"
        [ class "center-page" ]
        (List.map (\x -> viewKeyedElement x) (Dict.values model.quizElements))


viewKeyedElement : QuizElement -> ( String, Html Msg )
viewKeyedElement e =
    case e of
        SectionElement section ->
            viewKeyedSection section

        QuestionElement question ->
            viewKeyedQuestion question


viewKeyedSection : Section -> ( String, Html Msg )
viewKeyedSection s =
    ( String.fromInt s.id, lazy viewSection s )


viewSection : Section -> Html Msg
viewSection s =
    div [ class "QE-section" ]
        [ input
            [ type_ "text"
            , class "Stext"
            , value s.title
            , id (getSectionIDStr s)
            , Events.onInput (ChangeSection s)
            ]
            []
        , button [ Events.onClick (DeleteSection s) ] [ text "X" ]
        ]


viewKeyedQuestion : Question -> ( String, Html Msg )
viewKeyedQuestion q =
    ( String.fromInt q.id, lazy viewQuestion q )


viewQuestion : Question -> Html Msg
viewQuestion q =
    div [ class "QE-question", id (getQuestionIDStr q) ]
        [ input
            [ type_ "text"
            , class "Qtext"
            , value q.question
            , Events.onInput (ChangeQuestion q)
            ]
            []
        , button [ Events.onClick (DeleteQuestion q) ] [ text "X" ]
        , Keyed.node "div" [ class "QE-choices" ] (List.map (viewKeyedChoice q) (Dict.values q.choices))
        , div [ class "one-more-choice" ]
            [ button [ Events.onClick (InsertChoice q) ] [ text "+ choice" ]
            ]
        ]


viewKeyedChoice : Question -> Choice -> ( String, Html Msg )
viewKeyedChoice c q =
    ( getChoiceIDStr c q, lazy2 viewChoice q c )


viewChoice : Choice -> Question -> Html Msg
viewChoice c q =
    div [ class "QE-choice", id (getChoiceIDStr q c) ]
        [ input [ class "Ctext", type_ "text", value c.choice, Events.onInput (ChangeChoice q c) ] []
        , input
            [ type_ "radio"
            , class "Cradio"
            , name (getQuestionIDStr q)
            , Events.onClick (SetRightChoice q c)
            ]
            []
        , button [ Events.onClick (DeleteChoice q c) ] [ text "X" ]
        ]


viewPopup : Maybe Popup -> Html Msg
viewPopup popup =
    case popup of
        Nothing ->
            text ""

        Just (Popup str) ->
            div [ class "elm-popup" ] [ text str ]



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
    , popup : Maybe Popup
    }


type Popup
    = Popup String



-- types helpers


getNextElementID : Model -> Int
getNextElementID model =
    case model.lastIndex of
        Nothing ->
            0

        Just int ->
            1 + int



-- sections related


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


quizElementToBool : QuizElement -> Bool
quizElementToBool el =
    case el of
        QuestionElement _ ->
            True

        SectionElement _ ->
            False


isQuestionFound : Model -> Bool
isQuestionFound model =
    let
        elements =
            Dict.values model.quizElements
    in
    if List.isEmpty elements then
        False

    else
        let
            boolList =
                List.map quizElementToBool elements
        in
        -- if there is any questiom element the list will contain at least one True
        -- and then return True
        List.member True boolList


isChoiceNotFound : Question -> Bool
isChoiceNotFound q =
    Dict.isEmpty q.choices


questionsWithNoChoices : List Question -> List Question
questionsWithNoChoices qList =
    List.filter isChoiceNotFound qList


isAllQuestionsHaveChoices : Model -> Bool
isAllQuestionsHaveChoices model =
    let
        qList =
            extractQuestionsFromElements (Dict.values model.quizElements)

        qWithNoChoices =
            questionsWithNoChoices qList
    in
    List.isEmpty qWithNoChoices


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



-- Encoder


choiceEncoder : Question -> Choice -> Encode.Value
choiceEncoder q c =
    Encode.object
        [ ( "id", Encode.string (getChoiceIDStr q c) )
        , ( "choice", Encode.string c.choice )
        ]


sectionEncoder : Section -> Encode.Value
sectionEncoder s =
    Encode.object
        [ ( "id", Encode.string (getSectionIDStr s) )
        , ( "title", Encode.string s.title )
        ]


questionEncoder : Question -> Encode.Value
questionEncoder q =
    let
        idToDictKey :
            Int
            -> String -- ex: 0 -> e0c0, 1 -> e0c1   etc..
        idToDictKey k =
            getQuestionIDStr q ++ "c" ++ String.fromInt k

        rigthChoiceIDStr =
            getQuestionIDStr q ++ "c" ++ String.fromInt (Maybe.withDefault 0 q.rightChoice)
    in
    Encode.object
        [ ( "id", Encode.string (getQuestionIDStr q) )
        , ( "question", Encode.string q.question )
        , ( "choices", Encode.dict idToDictKey (choiceEncoder q) q.choices )
        , ( "rightChoice", Encode.string rigthChoiceIDStr )
        ]


quizElementEncoder : QuizElement -> Encode.Value
quizElementEncoder element =
    case element of
        QuestionElement q ->
            questionEncoder q

        SectionElement s ->
            sectionEncoder s


modelEncoder : Model -> Encode.Value
modelEncoder model =
    let
        idToDictKey : Int -> String
        idToDictKey k =
            "e" ++ String.fromInt k
    in
    Encode.object
        [ ( "quizTitle", Encode.string model.quizTitle )
        , ( "quizElements", Encode.dict idToDictKey quizElementEncoder model.quizElements )
        ]


generateJson : Model -> String
generateJson model =
    Encode.encode 1 (modelEncoder model)



-- commands


waitThenRemovePopup : Cmd Msg
waitThenRemovePopup =
    Task.perform RemovePopup (Process.sleep 1800)
