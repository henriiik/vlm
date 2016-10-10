module Buffer
    exposing
        ( Buffer
        , cut
        , get
        , insert
        , join
        , remove
        , set
        , split
        , splitLeft
        , splitRight
        )

import Array
import Cursor exposing (Cursor)
import Line exposing (Line)
import Selection exposing (Selection)


type alias Buffer =
    Array.Array Line


splitLeft : Int -> Buffer -> Buffer
splitLeft i b =
    Array.slice 0 i b


splitRight : Int -> Buffer -> Buffer
splitRight i b =
    Array.slice i (Array.length b) b


split : Cursor -> Buffer -> ( Buffer, Buffer )
split cur buf =
    let
        top =
            splitLeft cur.row buf

        bot =
            splitRight cur.row buf

        ( left, right ) =
            Line.split cur.col (get 0 bot)
    in
        ( Array.push left top, set 0 right bot )


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


last : Buffer -> String
last buf =
    get ((Array.length buf) - 1) buf


set : Int -> String -> Buffer -> Buffer
set i s b =
    Array.set i s b


join : Buffer -> Buffer -> Buffer
join a b =
    let
        i =
            (Array.length a) - 1

        line =
            (get i a) ++ (get 0 b)
    in
        Array.append (Array.slice 0 -1 a) (set 0 line b)


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
        let
            ( ab, c ) =
                split sel.end buf

            ( a, b ) =
                split sel.start ab
        in
            Debug.log "cut" ( join a c, b )
