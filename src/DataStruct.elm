module DataStruct exposing (..)

-- for experimentaion

import Json.Decode exposing (Decoder, field, list, map, map2, map5, nullable, string)



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
    , chosenChoice : Maybe String -- id of choice.
    , lastID : Int --  to keep track of the last question id to increment

    --  or decrement it when adding or deleting questions
    --  will not be encoded to json
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
    }



-- Decoders


choiceDecoder : Decoder Choice
choiceDecoder =
    map2 Choice (field "choiceID" string) (field "choice" string)


questionDecoder : Decoder Question
questionDecoder =
    map5 Question
        (field "questionID" string)
        (field "question" string)
        (field "choices" (list choiceDecoder))
        (field "correctChoice" string)
        (field "chosenChoice" (nullable string))


sectionDecoder : Decoder Section
sectionDecoder =
    map Section (field "sectionTitle" string)



{- quizElementDecoder : Decoder QuizElement
   quizElementDecoder =
-}


quizDecoder : Decoder Quiz
quizDecoder =
    map2 Quiz (field "quizTitle" string) (field "quizElement" (list sectionDecoder))



-- Encoders
