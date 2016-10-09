module Buffer
    exposing
        ( Buffer
        , Selection
        , cut
        , get
        , insert
        , remove
        , set
        , splitLeft
        , splitRight
        )

import Array
import Cursor exposing (Cursor)
import Line exposing (Line)


type alias Selection =
    { start : Cursor
    , end : Cursor
    }


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
    Array.append (Array.push s (splitLeft i b)) (splitRight i b)


remove : Int -> Buffer -> Buffer
remove i b =
    Array.append (splitLeft i b) (splitRight (i + 1) b)


get : Int -> Buffer -> String
get i b =
    b
        |> Array.get i
        |> Maybe.withDefault ""


set : Int -> String -> Buffer -> Buffer
set i s b =
    Array.set i s b


cut : Selection -> Buffer -> ( Buffer, Buffer )
cut sel buf =
    if sel.start.row == sel.end.row then
        let
            l =
                get sel.start.row buf

            ( ab, c ) =
                Line.split sel.end.col l

            ( a, b ) =
                Line.split sel.start.col ab
        in
            ( set sel.start.row (a ++ c) buf, (Array.repeat 1 b) )
    else
        ( buf, buf )
