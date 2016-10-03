module Buffer exposing (Buffer, splitLeft, splitRight, insert, replace)

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


replace : Int -> String -> Buffer -> Buffer
replace i s b =
    Array.set i s b
