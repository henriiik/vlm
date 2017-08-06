module Cursor
    exposing
        ( Cursor
        , withCol
        , withRow
        , cmp
        , left
        , right
        , up
        , down
        )


type alias Cursor =
    { row : Int
    , col : Int
    }


withCol : Int -> Cursor -> Cursor
withCol i c =
    { c | col = i }


withRow : Int -> Cursor -> Cursor
withRow i c =
    { c | row = i }


left : Cursor -> Cursor
left cur =
    { cur | col = cur.col - 1 }


right : Cursor -> Cursor
right cur =
    { cur | col = cur.col + 1 }


up : Cursor -> Cursor
up cur =
    { cur | row = cur.row - 1 }


down : Cursor -> Cursor
down cur =
    { cur | row = cur.row + 1 }


cmp : Cursor -> Cursor -> Order
cmp a b =
    if a.row > b.row then
        GT
    else if a.row < b.row then
        LT
    else
        compare a.col b.col
