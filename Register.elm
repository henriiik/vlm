module Register
    exposing
        ( Register(Normal, Line)
        , insert
        , cut
        , cutLines
        )

import Array
import Buffer exposing (Buffer)
import Cursor exposing (Cursor)
import Line exposing (Line)
import Selection exposing (Selection)
import String


type Register
    = Normal Buffer
    | Line Buffer


insert : Register -> Cursor -> Buffer -> ( Buffer, Cursor )
insert reg cur buf =
    case reg of
        Normal b ->
            let
                ( a, c ) =
                    Buffer.split cur buf

                ab =
                    Buffer.join a b

                row =
                    Array.length ab - 1

                col =
                    (String.length (Buffer.get row ab)) - 1
            in
                ( Buffer.join ab c, Cursor row col )

        Line b ->
            let
                a =
                    Buffer.splitLeft cur.row buf

                c =
                    Buffer.splitRight cur.row buf
            in
                ( Array.append (Array.append a b) c, Cursor.withCol 0 cur )


cut : Selection -> Buffer -> ( Buffer, Register )
cut sel buf =
    if sel.start.row == sel.end.row then
        let
            l =
                Buffer.get sel.start.row buf

            ( ab, c ) =
                Line.split sel.end.col l

            ( a, b ) =
                Line.split sel.start.col ab
        in
            ( Buffer.set sel.start.row (a ++ c) buf, Normal (Array.repeat 1 b) )
    else
        let
            ( ab, c ) =
                Buffer.split sel.end buf

            ( a, b ) =
                Buffer.split sel.start ab
        in
            Debug.log "cut" ( Buffer.join a c, Normal b )


cutLines : Selection -> Buffer -> ( Buffer, Register )
cutLines sel buf =
    let
        i =
            sel.end.row + 1

        a =
            Buffer.splitLeft sel.start.row buf

        b =
            Array.slice sel.start.row i buf

        c =
            Buffer.splitRight i buf
    in
        Debug.log "cutLines" ( Array.append a c, Line b )
