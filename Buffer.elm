module Buffer
    exposing
        ( Buffer
        , Line
        , Selection
        , get
        , insert
        , remove
        , set
        , select
        , splitLeft
        , splitRight
        )

import Array


type alias Selection =
    { start : Int
    , end : Int
    }


type alias Line =
    ( String, Maybe Selection )


type alias Buffer =
    Array.Array Line


splitLeft : Int -> Buffer -> Buffer
splitLeft i b =
    Array.slice 0 i b


splitRight : Int -> Buffer -> Buffer
splitRight i b =
    Array.slice i (Array.length b) b


insert : Int -> String -> Buffer -> Buffer
insert i s b =
    Array.append (Array.push ( s, Nothing ) (splitLeft i b)) (splitRight i b)


remove : Int -> Buffer -> Buffer
remove i b =
    Array.append (splitLeft i b) (splitRight (i + 1) b)


get : Int -> Buffer -> String
get i b =
    b
        |> Array.get i
        |> Maybe.map fst
        |> Maybe.withDefault ""


set : Int -> String -> Buffer -> Buffer
set i s b =
    Array.set i ( s, Nothing ) b


select : Int -> Selection -> Buffer -> Buffer
select i s b =
    case Array.get i b of
        Just omg ->
            Array.set i ( fst omg, Just s ) b

        _ ->
            b
