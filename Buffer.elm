module Buffer
    exposing
        ( Buffer
        , get
        , insert
        , remove
        , replace
        , set
        , splitLeft
        , splitRight
        )

import Array


type alias Buffer =
    Array.Array String


splitLeft : Int -> Buffer -> Buffer
splitLeft i b =
    Array.slice 0 i b


splitRight : Int -> Buffer -> Buffer
splitRight i b =
    Array.slice i (Array.length b) b


insert : Int -> String -> Buffer -> Buffer
insert i s b =
    Array.append (Array.push s (splitLeft i b)) (splitRight i b)


remove : Int -> Buffer -> Buffer
remove i b =
    Array.append (splitLeft i b) (splitRight (i + 1) b)


replace : Int -> String -> Buffer -> Buffer
replace i s b =
    Array.set i s b


get : Int -> Buffer -> String
get i b =
    b
        |> Array.get i
        |> Maybe.withDefault ""


set : Int -> String -> Buffer -> Buffer
set i s b =
    Array.set i s b
