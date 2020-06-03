module Encoder exposing (..)

import Json.Encode exposing (..)
import Model exposing (..)


letters : Letters -> Value
letters ls =
    dict identity int ls


word : Word -> Value
word w =
    object
        [ ( "string", string w.string )
        , ( "letters", letters w.letters )
        ]


pair : ( Int, Int ) -> Value
pair ( x, y ) =
    list int [ x, y ]


pool : Pool -> Value
pool p =
    object
        [ ( "order", list string p.order )
        , ( "letters", letters p.letters )
        ]


unflipped : List String -> Value
unflipped u =
    list string u


player : Player -> Value
player p =
    object
        [ ( "name", string p.name )
        , ( "score", int p.score )
        , ( "words", list word p.words )
        ]


move : Move -> Value
move m =
    object
        [ ( "thief", string m.thief )
        , ( "victim", string m.victim )
        , ( "target", string m.target )
        , ( "stolen", string m.stolen )
        ]


state : State -> Value
state s =
    object
        [ ( "pool", pool s.pool )
        , ( "unflipped", unflipped s.unflipped )
        , ( "numplayers", int s.numPlayers )
        , ( "players", list player s.players )
        , ( "history", list move s.history )
        ]


all : Model -> Value
all m =
    object
        [ ( "pool", pool m.pool )
        , ( "unflipped", unflipped m.unflipped )
        , ( "numplayers", int m.numPlayers )
        , ( "players", list player m.players )
        , ( "history", list move m.history )
        , ( "prevstate"
          , case m.prevState of
                Nothing ->
                    null

                Just s ->
                    state s
          )
        ]
