module Cursor exposing (Cursor, withCol, withRow, cmp)


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


cmp : Cursor -> Cursor -> Order
cmp a b =
    if a.row > b.row then
        GT
    else if a.row < b.row then
        LT
    else
        compare a.col b.col
