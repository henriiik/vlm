module Selection
    exposing
        ( Selection
        , fromCursors
        )

import Cursor exposing (Cursor)


type alias Selection =
    { start : Cursor
    , end : Cursor
    }


fromCursors : Cursor -> Cursor -> Selection
fromCursors a b =
    case Cursor.cmp a b of
        LT ->
            Selection a (Cursor.right b)

        _ ->
            Selection b (Cursor.right a)
