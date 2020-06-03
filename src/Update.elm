port module Update exposing (..)

import Browser.Events
import CharMap exposing (..)
import Decoder as D
import Encoder as E
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra
import Model exposing (..)
import Random
import Random.Char
import Random.String



-- Subscriptions


port firebaseWrite : OutsideInfo -> Cmd msg


port firebaseRead : (OutsideInfo -> msg) -> Sub msg


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map
        (\s ->
            case s of
                " " ->
                    Flip

                "Enter" ->
                    Steal

                "Backspace" ->
                    Backspace

                c ->
                    case String.uncons c of
                        Just ( x, "" ) ->
                            Typing (String.fromChar x)

                        _ ->
                            NoOp
        )
        (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ firebaseRead ReceiveValue
        , if model.joined then
            Browser.Events.onKeyDown keyDecoder

          else
            Sub.none
        ]


sendData : Model -> Cmd msg
sendData m =
    firebaseWrite { tag = "write", path = m.roomCode, data = E.all m }


addNumPlayers : Model -> Cmd msg
addNumPlayers m =
    firebaseWrite
        { tag = "increment"
        , path = m.roomCode ++ "/numplayers"
        , data = Encode.null
        }


sendVote : Model -> Cmd msg
sendVote m =
    firebaseWrite
        { tag = "increment"
        , path = m.roomCode ++ "/challenges"
        , data = Encode.null
        }


type alias OutsideInfo =
    { tag : String, path : String, data : Value }



-- Update


type Msg
    = NoOp
    | Load
    | Create
    | TypingRoom String
    | TypingName String
    | Typing String
    | Backspace
    | Flip
    | Steal
    | Challenge
    | ReceiveValue OutsideInfo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Load ->
            if model.myName == "" then
                ( { model | log = "Please enter your name" }, Cmd.none )

            else if model.roomCode == "" then
                ( { model | log = "Please enter a room code" }, Cmd.none )

            else
                ( { model | loading = True, log = "" }
                , addNumPlayers model
                )

        Create ->
            if model.myName == "" then
                ( { model | log = "Please enter your name" }, Cmd.none )

            else
                let
                    ( randomRoom, seed ) =
                        Random.step
                            (Random.String.string 5 Random.Char.lowerCaseLatin)
                            model.randomSeed
                in
                ( { model
                    | typing = ""
                    , log = ""
                    , joined = True
                    , randomSeed = seed
                    , roomCode = randomRoom
                  }
                , sendData
                    { model
                        | numPlayers = 1
                        , players = [ { name = model.myName, words = [], score = 0 } ]
                        , roomCode = randomRoom
                    }
                )

        TypingRoom room ->
            ( { model | roomCode = room }, Cmd.none )

        TypingName name ->
            ( { model | myName = name }, Cmd.none )

        Typing word ->
            if model.joined then
                ( { model | typing = model.typing ++ word }, Cmd.none )

            else
                ( model, Cmd.none )

        Backspace ->
            ( { model
                | typing = String.slice 0 -1 model.typing
              }
            , Cmd.none
            )

        Flip ->
            let
                flipmodel =
                    flipLetter model
            in
            ( { model | randomSeed = flipmodel.randomSeed, log = flipmodel.log }, sendData flipmodel )

        Steal ->
            let
                stealmodel =
                    stealWord model
            in
            ( { model | typing = "", log = stealmodel.log }, sendData stealmodel )

        Challenge ->
            ( { model | canChallenge = False }, sendVote model )

        ReceiveValue { tag, path, data } ->
            if not (model.joined || model.loading) then
                -- ignore updates before we join a room
                ( model, Cmd.none )

            else
                let
                    newM =
                        case
                            Decode.decodeValue (Decode.field model.roomCode D.all) data
                        of
                            Ok d ->
                                { model
                                    | pool = d.pool
                                    , unflipped = d.unflipped
                                    , numPlayers = d.numPlayers
                                    , players = d.players
                                    , prevState = d.prevState
                                    , history = d.history
                                    , log = ""
                                    , challenges = d.challenges
                                    , canChallenge =
                                        -- allow new move to be challenged until new flip
                                        (model.canChallenge && (model.pool == d.pool))
                                            || (List.drop 1 d.history == model.history)
                                }

                            Err e ->
                                let
                                    _ =
                                        Debug.log "error receiving values" e
                                in
                                model
                in
                if newM.challenges > newM.numPlayers // 2 then
                    -- challenge succeeded, undo last move
                    let
                        restoredModel =
                            restore newM
                    in
                    ( restoredModel, sendData restoredModel )

                else if model.loading then
                    -- finish loading room
                    ( { newM
                        | typing = ""
                        , joined = True
                        , loading = False
                        , me = newM.numPlayers - 1
                      }
                    , sendData
                        { newM
                            | players =
                                newM.players
                                    ++ [ { name = model.myName
                                         , words = []
                                         , score = 0
                                         }
                                       ]
                        }
                    )

                else
                    ( newM, Cmd.none )



-- Gameplay


restore : Model -> Model
restore m =
    case m.prevState of
        Nothing ->
            m

        Just ps ->
            { m
                | prevState = Nothing
                , challenges = 0
                , canChallenge = False
                , pool = ps.pool
                , unflipped = ps.unflipped
                , players = ps.players
                , numPlayers = ps.numPlayers
                , history = ps.history
            }


flipLetter : Model -> Model
flipLetter model =
    let
        len =
            List.length model.unflipped

        ( i, seed ) =
            Random.step (Random.int 0 (len - 1)) model.randomSeed

        c =
            case List.Extra.getAt i model.unflipped of
                Nothing ->
                    ""

                Just x ->
                    x
    in
    if len == 0 then
        { model | log = "No more letters" }

    else
        { model
            | pool =
                { order = model.pool.order ++ [ c ]
                , letters =
                    updateOrDefault c
                        ((+) 1)
                        1
                        model.pool.letters
                }
            , unflipped = List.Extra.removeAt i model.unflipped
            , randomSeed = seed
            , log = ""
        }


stringToLetters : String -> Letters
stringToLetters s =
    let
        chars =
            String.split "" (String.toUpper s)
    in
    List.foldl
        (\x l ->
            updateOrDefault x ((+) 1) 1 l
        )
        empty
        chars


score : Word -> Int
score w =
    String.length w.string - 2


stealWord : Model -> Model
stealWord model =
    let
        targetString =
            String.toUpper model.typing

        targetLetters =
            stringToLetters targetString

        targetWord =
            { string = targetString
            , letters = targetLetters
            }
    in
    if String.length targetString < 4 then
        { model | log = "Word too short" }

    else
        case poolOrPlayersSteal targetWord model.players model.myName model.pool of
            Nothing ->
                { model | typing = "", log = "Letters not available" }

            Just ( move, newPlayers, newPool ) ->
                { model
                    | prevState = Just (modelToState model)
                    , players =
                        List.Extra.updateAt model.me
                            (\p ->
                                { p
                                    | words = targetWord :: p.words
                                    , score = p.score + score targetWord
                                }
                            )
                            newPlayers
                    , pool = newPool
                    , history = move :: model.history
                    , log = ""
                    , challenges = 0
                }


poolOrPlayersSteal : Word -> List Player -> String -> Pool -> Maybe ( Move, List Player, Pool )
poolOrPlayersSteal target players me pool =
    case poolSteal target pool of
        Just newPool ->
            Just
                ( { thief = me
                  , victim = ""
                  , target = target.string
                  , stolen = ""
                  }
                , players
                , newPool
                )

        Nothing ->
            playersSteal target players me pool


playersSteal : Word -> List Player -> String -> Pool -> Maybe ( Move, List Player, Pool )
playersSteal targetWord players me pool =
    let
        target =
            targetWord.letters
    in
    case players of
        [] ->
            Nothing

        p :: ps ->
            case findSteal target p.words pool of
                Nothing ->
                    case playersSteal targetWord ps me pool of
                        Nothing ->
                            Nothing

                        Just ( move, newps, pl ) ->
                            Just ( move, p :: newps, pl )

                Just ( otherwords, stolenword, newpool ) ->
                    let
                        move =
                            { thief = me
                            , victim = p.name
                            , target = targetWord.string
                            , stolen = stolenword.string
                            }
                    in
                    Just
                        ( move, { p | words = otherwords, score = p.score - score stolenword } :: ps, newpool )


findSteal : Letters -> List Word -> Pool -> Maybe ( List Word, Word, Pool )
findSteal target words pool =
    case words of
        [] ->
            Nothing

        w :: rest ->
            case validSteal target w.letters pool of
                Nothing ->
                    case findSteal target rest pool of
                        Nothing ->
                            Nothing

                        Just ( ws, wrd, p ) ->
                            Just ( w :: ws, wrd, p )

                Just newPool ->
                    Just ( rest, w, newPool )


removeN : String -> Int -> List String -> List String
removeN s n lis =
    if n <= 0 then
        lis

    else
        case lis of
            [] ->
                []

            x :: xs ->
                if x == s then
                    removeN s (n - 1) xs

                else
                    x :: removeN s n xs


poolSteal : Word -> Pool -> Maybe Pool
poolSteal target pool =
    validSteal target.letters CharMap.empty pool


validSteal : Letters -> Letters -> Pool -> Maybe Pool
validSteal target candidate pool =
    case
        foldl
            (\k _ b ->
                case b of
                    Nothing ->
                        Nothing

                    Just ( u, p ) ->
                        let
                            targetcount =
                                getCount k target

                            poolcount =
                                getCount k pool.letters

                            candcount =
                                getCount k candidate

                            lettersAvailable =
                                targetcount <= candcount + poolcount && candcount <= targetcount

                            poolUsed =
                                u || targetcount /= candcount
                        in
                        if lettersAvailable then
                            Just
                                ( poolUsed
                                , { order = removeN k (targetcount - candcount) p.order
                                  , letters = sub k (targetcount - candcount) p.letters
                                  }
                                )

                        else
                            Nothing
            )
            (Just ( False, pool ))
            -- we have to fold over the union to make sure we check that
            -- the condition holds for each letter
            (union
                target
                candidate
            )
    of
        Nothing ->
            Nothing

        Just ( True, p ) ->
            Just p

        Just ( False, p ) ->
            Nothing
