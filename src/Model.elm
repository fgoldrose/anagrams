module Model exposing (..)

import CharMap exposing (..)
import Random



-- Types


type alias Letters =
    CharMap


type alias Word =
    { string : String
    , letters : Letters
    }


type alias Player =
    { name : String
    , words : List Word
    , score : Int
    }


type alias Pool =
    { order : List String
    , letters : Letters
    }


type alias Attempt =
    { target : Word
    , stolen : Maybe Word
    , newPlayers : List Player
    , newPool : Pool
    , aye : Int
    , nay : Int
    }



-- has to be a type to allow recursive definition


type alias Move =
    --= Move
    { thief : String
    , victim : String
    , target : String
    , stolen : String
    }


type alias State =
    { pool : Pool -- shared vvv
    , unflipped : List String
    , players : List Player
    , numPlayers : Int
    , history : List Move -- shared ^^^
    }


type alias WithPrev a =
    { a | prevState : Maybe State, challenges : Int }


modelToState : Model -> State
modelToState model =
    { pool = model.pool
    , unflipped = model.unflipped
    , players = model.players
    , numPlayers = model.numPlayers
    , history = model.history
    }


type alias Model =
    { roomCode : String
    , pool : Pool -- shared vvv
    , unflipped : List String
    , players : List Player
    , numPlayers : Int
    , challenges : Int
    , prevState : Maybe State
    , history : List Move -- shared ^^^
    , typing : String -- private vvv
    , me : Int
    , myName : String
    , joined : Bool
    , loading : Bool
    , randomSeed : Random.Seed
    , canChallenge : Bool
    , log : String -- private ^^^
    }
