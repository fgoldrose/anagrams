module CharMap exposing (..)

import Dict


type alias CharMap =
    Dict.Dict String Int


empty : CharMap
empty =
    Dict.empty


getCount : String -> CharMap -> Int
getCount c m =
    case Dict.get c m of
        Nothing ->
            0

        Just v ->
            v


updateOrDefault : String -> (Int -> Int) -> Int -> CharMap -> CharMap
updateOrDefault c f d m =
    Dict.update c
        (\x ->
            case x of
                Nothing ->
                    Just d

                Just v ->
                    Just (f v)
        )
        m


sub : String -> Int -> CharMap -> CharMap
sub c i m =
    Dict.update c
        (\x ->
            case x of
                Nothing ->
                    Nothing

                Just v ->
                    Just
                        (if v - i > 0 then
                            v - i

                         else
                            0
                        )
        )
        m


foldl : (String -> Int -> b -> b) -> b -> CharMap -> b
foldl =
    Dict.foldl


union : CharMap -> CharMap -> CharMap
union m1 m2 =
    Dict.union m1 m2
