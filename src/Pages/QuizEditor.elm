module Pages.QuizEditor exposing (Model, init, initModel, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Quiz



-- create initial model


defaultQuestion : Question
defaultQuestion =
    Question "q1" "Question 1 ......" defaultChoices "q1c1" Nothing


defaultChoices : List Choice
defaultChoices =
    [ Choice "q1c1" "choice A", Choice "q1c2" "choice B" ]


initModel : Model
initModel =
    Quiz "quiz 1" [ QuestionElement defaultQuestion ] 0 0


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Browser.Document Msg
view model =
    { title = "Malaz Quiz Editor"
    , body = [ viewHeader model, viewMain model, viewFooter ]
    }


viewHeader : Quiz -> Html Msg
viewHeader quiz =
    header [] [ input [ type_ "textbox", value quiz.quizTitle ] [] ]


viewMain : Quiz -> Html Msg
viewMain quiz =
    main_ [] (List.map (\x -> viewElement x) quiz.quizElements)


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
    h2 [] [ text section.sectionTitle ]


viewQuestion : Question -> Html Msg
viewQuestion question =
    div [ class "question" ]
        [ div [ class "Qtext", id "" ] [ text question.question ]
        , div [ class "Qchoices", id "" ] []
        ]


viewChoice : Question -> Html Msg
viewChoice question =
    div [ class "Qchoice" ]
        [ label [ for "" ] []
        , input [ type_ "radio", id "" ] []
        ]



-- Types


type alias Choice =
    { choiceID : String
    , choice : String
    }


type alias Question =
    { questionID : String
    , question : String
    , choices : List Choice
    , correctChoice : String -- id of choice.
    , chosenChoice : Maybe String -- id of choice, will be encoded to null
    }


type alias Section =
    { sectionTitle : String
    , sectionID : Int
    }


type QuizElement
    = QuestionElement Question
    | SectionElement Section


type alias Quiz =
    { quizTitle : String
    , quizElements : List QuizElement
    , lastQuestionID : Int -- to keep track of the last question id to increment
    , lastSectionID : Int -- or decrement it when adding or deleting questions

    -- will not be encoded to json
    }
