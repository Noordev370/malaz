module Pages.QuizTake exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (name, style, type_, value)
import Html.Events exposing (onClick)


type Model
    = BeforeSubmit Quiz
    | AfterSubmit Quiz


type Msg
    = Submit



-- Dummy quiz for dev purposes


dummyQuiz : Quiz
dummyQuiz =
    Quiz "organic lec 1" [ dumyySection ]


dumyySection : Section
dumyySection =
    Section "section Alchols" "s1" [ dummyQuestion1, dummyQuestion2 ]


dummyQuestion1 : Question
dummyQuestion1 =
    Question "q1"
        "Alchol is ...."
        [ { choiceID = "q1c1", choice = "Alchol" }
        , { choiceID = "q1c2", choice = "Ether" }
        , { choiceID = "q1c3", choice = "Ester" }
        ]
        "Alchol"
        Nothing


dummyQuestion2 : Question
dummyQuestion2 =
    Question "q2"
        "Ethan is ...."
        [ { choiceID = "q2c1", choice = "Alkene" }
        , { choiceID = "q2c2", choice = "Alkan" }
        , { choiceID = "q2c3", choice = "Alkyn" }
        ]
        "Alkan"
        Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initModel =
            dummyQuiz
    in
    ( BeforeSubmit initModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( AfterSubmit dummyQuiz, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model of
        BeforeSubmit quiz ->
            { title = quiz.quizTitle
            , body = [ viewHeader quiz, viewMain model, viewFooter ]
            }

        AfterSubmit quiz ->
            { title = "Result of " ++ quiz.quizTitle
            , body = [ viewHeader quiz, hr [] [], viewMain model, hr [] [], viewFooter ]
            }


viewHeader : Quiz -> Html Msg
viewHeader quiz =
    header []
        [ h1 [] [ text quiz.quizTitle ]
        ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ h3 [] [ text "Malaz" ]
        , text "by Noor Eldeen"
        ]


viewMain : Model -> Html Msg
viewMain model =
    case model of
        BeforeSubmit quiz ->
            main_ []
                (List.concat
                    [ [ text quiz.quizTitle ]
                    , List.map (\x -> viewSection x) quiz.sections
                    , [ button [ onClick Submit ] [ text "button" ] ]
                    ]
                )

        AfterSubmit quiz ->
            text "After"


viewChoices : List Choice -> List (Html Msg)
viewChoices choices =
    List.map (\x -> input [ type_ "radio", name "111" ] [ text <| x.choice ++ " " ]) choices


viewQuestion : Question -> Html Msg
viewQuestion question =
    div []
        [ div [] [ text question.question ]
        , div [] (viewChoices question.choices)
        , div [] [ "Correct ans is: " ++ question.correctChoice |> text ]
        , hr [ style "color" "green" ] []
        ]


viewSection : Section -> Html Msg
viewSection section =
    div []
        [ div [] [ text section.sectionTitle ]
        , div [] (List.map (\x -> viewQuestion x) section.questions)
        , hr [ style "color" "brown" ] []
        ]



-- Utils & Helpers


getQuestionsOfSections : List Section -> List Question
getQuestionsOfSections sections =
    List.concatMap (\x -> x.questions) sections


getSectionsOfQuiz : Quiz -> List Section
getSectionsOfQuiz quiz =
    quiz.sections


getQuestionsOfQuiz : Quiz -> List Question
getQuestionsOfQuiz quiz =
    quiz |> getSectionsOfQuiz |> getQuestionsOfSections



-- spiral [1,3,5] [2,4,8] == [1,2,3,4,5]
-- spiral ["a","c","e"] ["b","d","f"]


spiral : List a -> List a -> List a
spiral xs ys =
    let
        l1 =
            xs

        l2 =
            List.map (\x -> List.singleton x) ys
    in
    List.map2 (::) l1 l2 |> List.concat
