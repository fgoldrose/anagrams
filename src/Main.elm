module Main exposing (..)

import Browser
import Browser.Events
import CharMap
import Model
import Random
import Random.List
import Update
import View


main : Program Flags Model.Model Update.Msg
main =
    Browser.element
        { init = init
        , update = Update.update
        , subscriptions = Update.subscriptions
        , view = View.view
        }



-- Init


type alias Flags =
    { timeNow : Int
    }


init : Flags -> ( Model.Model, Cmd Update.Msg )
init flags =
    ( { roomCode = ""
      , pool = { order = [], letters = CharMap.empty }
      , unflipped = allLetters
      , numPlayers = 0
      , players = []
      , challenges = 0
      , history = []
      , prevState = Nothing
      , typing = ""
      , me = 0
      , myName = ""
      , joined = False
      , loading = False
      , randomSeed = Random.initialSeed flags.timeNow
      , canChallenge = True
      , log = ""
      }
    , Cmd.none
    )


allLetters : List String
allLetters =
    List.repeat 13 "A"
        ++ List.repeat 5 "B"
        ++ List.repeat 6 "C"
        ++ List.repeat 7 "D"
        ++ List.repeat 24 "E"
        ++ List.repeat 6 "F"
        ++ List.repeat 7 "G"
        ++ List.repeat 6 "H"
        ++ List.repeat 12 "I"
        ++ List.repeat 2 "J"
        ++ List.repeat 2 "K"
        ++ List.repeat 8 "L"
        ++ List.repeat 8 "M"
        ++ List.repeat 11 "N"
        ++ List.repeat 15 "O"
        ++ List.repeat 4 "P"
        ++ List.repeat 2 "Q"
        ++ List.repeat 12 "R"
        ++ List.repeat 10 "S"
        ++ List.repeat 10 "T"
        ++ List.repeat 6 "U"
        ++ List.repeat 2 "V"
        ++ List.repeat 4 "W"
        ++ List.repeat 2 "X"
        ++ List.repeat 2 "Y"
        ++ List.repeat 2 "Z"
