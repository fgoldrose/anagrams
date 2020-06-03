module Decoder exposing (..)

import CharMap exposing (CharMap)
import Json.Decode exposing (..)
import Model exposing (..)


letters : Decoder Letters
letters =
    dict int


pool : Decoder Pool
pool =
    map2 (\o l -> { order = o, letters = l })
        (oneOf [ field "order" (list string), succeed [] ])
        (field "letters" letters)


word : Decoder Word
word =
    map2 (\s l -> { string = s, letters = l })
        (field "string" string)
        (field "letters" letters)


unflipped : Decoder (List String)
unflipped =
    oneOf [ list string, succeed [] ]


player : Decoder Player
player =
    map3 (\n s w -> { name = n, score = s, words = w })
        (field "name" string)
        (field "score" int)
        (oneOf
            [ field "words" (list word), succeed [] ]
        )


move : Decoder Move
move =
    map4
        (\t v w s ->
            { target = w
            , stolen = s
            , victim = v
            , thief = t
            }
        )
        (field "thief" string)
        (field "victim" string)
        (field "target" string)
        (field "stolen" string)


state : Decoder State
state =
    map5 (\f u n p h -> { pool = f, unflipped = u, numPlayers = n, players = p, history = h })
        (oneOf [ field "pool" pool, succeed { order = [], letters = CharMap.empty } ])
        (oneOf
            [ field "unflipped" unflipped, succeed [] ]
        )
        (field "numplayers" int)
        (oneOf
            [ field "players" (list player), succeed [] ]
        )
        (oneOf
            [ field "history" (list move), succeed [] ]
        )


all : Decoder (WithPrev State)
all =
    map7 (\f u n p h s c -> { pool = f, unflipped = u, numPlayers = n, players = p, history = h, prevState = s, challenges = c })
        (oneOf [ field "pool" pool, succeed { order = [], letters = CharMap.empty } ])
        (oneOf
            [ field "unflipped" unflipped, succeed [] ]
        )
        (field "numplayers" int)
        (oneOf
            [ field "players" (list player), succeed [] ]
        )
        (oneOf
            [ field "history" (list move), succeed [] ]
        )
        (maybe (field "prevstate" state))
        (oneOf [ field "challenges" int, succeed 0 ])
