module Cursor exposing (Cursor, withCol, withRow)


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
