module Register
    exposing
        ( Register(Normal, Line)
        , insert
        )

import Buffer exposing (Buffer)
import Cursor exposing (Cursor)
import Array
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
