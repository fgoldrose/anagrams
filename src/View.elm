module View exposing (..)

import CharMap
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events
import Html.Events.Extra
import List.Extra
import Model exposing (..)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    if model.joined then
        playingView model

    else if model.loading then
        loadingView model

    else
        startView model


startView : Model -> Html Msg
startView model =
    Html.div [ style "font-family" "Arial", style "text-align" "center" ]
        [ Html.div [ style "font-size" "50px" ] [ Html.text "Anagrams" ]
        , Html.div [ style "font-size" "20px" ] [ Html.text "To start, create or join a game room" ]
        , Html.div [ style "margin" "10px" ]
            [ Html.input
                [ placeholder "Name"
                , value model.myName
                , Html.Events.onInput TypingName
                , style "font-size" "30px"
                , style "margin" "5px"
                , style "text-align" "center"
                ]
                []
            ]
        , Html.div []
            [ Html.div []
                [ Html.button
                    [ Html.Events.onClick Create
                    , style "background-color" "lightgreen"
                    , style "color" "white"
                    , style "font-size" "20px"
                    , style "margin" "5px"
                    , style "border" "none"
                    , style "border-radius" "5px"
                    ]
                    [ Html.text "Create Game" ]
                ]
            , Html.text "OR"
            , Html.div [ style "margin" "5px" ]
                [ Html.input
                    [ placeholder "Room Code"
                    , value model.roomCode
                    , Html.Events.onInput TypingRoom
                    ]
                    []
                , Html.button [ Html.Events.onClick Load ] [ Html.text "Join Game" ]
                ]
            ]
        , viewLog model
        ]


loadingView : Model -> Html Msg
loadingView model =
    Html.div [ style "font-family" "Arial" ]
        [ Html.text "Loading" ]


playingView : Model -> Html Msg
playingView model =
    Html.div
        [ style "font-family" "Arial"
        , style "text-align" "center"
        ]
        [ Html.div []
            [ Html.div [] [ headerView model, viewHistory model ]
            , viewLog model
            , viewPool model
            , viewWords model
            ]
        ]


headerView : Model -> Html Msg
headerView m =
    Html.div
        [ style "clear" "both"
        , style "text-align" "center"
        , style "margin-bottom" "20px"
        , style "width" "70%"
        ]
        [ Html.div [ style "font-size" "50px" ] [ Html.text "Anagrams" ]
        , Html.div [ style "font-size" "25px" ] [ Html.text ("Room Code: " ++ m.roomCode) ]
        , Html.div
            [ style "border" "2px dashed lightgray"
            , style "height" "30px"
            , style "width" "200px"
            , style "color"
                (if m.typing == "" then
                    "lightgray"

                 else
                    "black"
                )
            , style "text-align" "center"
            , style "margin" "auto"
            , style "padding-top" "5px"
            ]
            [ if m.typing == "" then
                Html.text "Type to steal word"

              else
                Html.div []
                    [ Html.text m.typing
                    ]
            ]
        , Html.div []
            [ Html.button
                [ Html.Events.onClick Flip
                , style "font-size" "20px"
                , style "background-color" "lightgreen"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "5px"
                , style "margin" "5px"
                ]
                [ Html.text "Flip" ]
            , Html.text "(or press space)"
            ]
        ]


viewMove : Move -> Html Msg
viewMove move =
    Html.div [ style "border-bottom" "2px dotted gray" ]
        [ if move.stolen == "" then
            Html.text (move.thief ++ " took " ++ move.target ++ " from the pool")

          else
            Html.text (move.thief ++ " made " ++ move.target ++ " from " ++ move.stolen)

        --Html.text (move.thief ++ " stole " ++ move.stolen ++ " from " ++ move.victim ++ " to make " ++ move.target)
        ]


viewHistory : Model -> Html Msg
viewHistory m =
    Html.div
        [ style "float" "right"
        , style "position" "absolute"
        , style "top" "10px"
        , style "right" "0px"
        , style "height" "100px"
        , style "width" "30%"
        , style "font-size" "20px"
        , style "margin" "10px"
        ]
        [ Html.text "History: "
        , Html.div
            [ style "border" "2px solid darkgreen"
            , style "overflow" "scroll"
            , style "font-size" "15px"
            , style "margin" "5px"
            , style "height" "-webkit-fill-available"
            , style "text-align" "left"
            , style "padding" "3px"
            ]
            (Html.div
                []
                [ if m.canChallenge && m.history /= [] then
                    Html.button
                        [ Html.Events.onClick Challenge
                        , style "font-size" "10px"
                        , style "background-color" "orange"
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "5px"
                        , style "float" "right"
                        ]
                        [ Html.text "Challenge" ]

                  else
                    Html.text ""
                ]
                :: List.map viewMove m.history
            )
        ]


viewLog : Model -> Html Msg
viewLog m =
    Html.div [ style "font-size" "20px", style "height" "30px", style "color" "orange" ] [ Html.text m.log ]


viewPool : Model -> Html Msg
viewPool m =
    let
        pool =
            m.pool
    in
    Html.div
        [ style "clear" "both"
        , style "margin-left" "20px"
        , style "min-height" "35px"
        ]
        (List.map tile
            pool.order
        )


tile : String -> Html Msg
tile l =
    Html.div
        [ style "background-color" "grey"
        , style "height" "35px"
        , style "width" "35px"
        , style "margin" "1px"
        , style "font-size" "30px"
        , style "color" "white"
        , style "text-align" "center"
        , style "float" "left"
        , style "border-radius" "5px"
        ]
        [ Html.text l ]


stringToTiles : String -> Html Msg
stringToTiles s =
    Html.div
        [ style "clear" "both"
        , style "padding-top" "2px"
        , style "padding-bottom" "2px"
        ]
        (String.foldr (\x l -> tile (String.fromChar x) :: l) [] s)


viewWords : Model -> Html Msg
viewWords m =
    Html.div [ style "clear" "both", style "padding" "10px" ]
        (List.indexedMap (\i p -> playerDiv p (i == m.me)) m.players)


playerDiv : Player -> Bool -> Html Msg
playerDiv p me =
    Html.div
        [ style "float" "left"
        , style "border"
            (if me then
                "2px solid lightgreen"

             else
                "2px dotted black"
            )
        , style "margin" "5px"
        , style "text-align" "center"
        , style "min-width" "200px"
        , style "min-height" "50px"
        , style "padding" "2px"
        ]
        (Html.div [] [ Html.text p.name ]
            :: Html.div [] [ Html.text (" Score: " ++ String.fromInt p.score) ]
            :: List.map (\w -> stringToTiles w.string) p.words
        )
