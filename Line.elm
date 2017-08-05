module Line
    exposing
        ( Line
        , deleteChar
        , insert
        , split
        )

import String


type alias Line =
    String


split : Int -> Line -> ( String, String )
split i a =
    ( String.left i a, String.right ((String.length a) - i) a )


deleteChar : Int -> Line -> Line
deleteChar i a =
    let
        s =
            split i a
    in
        (String.dropRight 1 (Tuple.first s)) ++ Tuple.second s


insert : Int -> String -> String -> String
insert i a b =
    let
        s =
            split i b
    in
        Tuple.first s ++ a ++ Tuple.second s
