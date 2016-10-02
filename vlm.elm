module Main exposing (..)

import Char
import Html exposing (..)
import Html.App
import Html.Attributes exposing (style)
import Keyboard
import String
import Array
import Regex
import Debug


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Mode
    = Normal
    | Insert


type alias Cursor =
    { row : Int
    , col : Int
    }


type alias Buffer =
    Array.Array String


type alias Editor =
    { cursor : Cursor
    , buffer : Buffer
    , width : Int
    , height : Int
    }


type alias Model =
    { editor : Editor
    , log : String
    , mode : Mode
    , ctrl : Bool
    , shift : Bool
    , alt : Bool
    }


left : Editor -> Editor
left editor =
    let
        col =
            (max (cursor.col - 1) 0)

        cursor =
            editor.cursor
    in
        { editor | cursor = { cursor | col = col } }


right : Editor -> Editor
right editor =
    let
        col =
            (min (cursor.col + 1) editor.width)

        cursor =
            editor.cursor
    in
        { editor | cursor = { cursor | col = col } }


down : Editor -> Editor
down editor =
    let
        row =
            (min (cursor.row + 1) (editor.height - 1))

        cursor =
            editor.cursor
    in
        { editor | cursor = { cursor | row = row } }


up : Editor -> Editor
up editor =
    let
        row =
            (max (cursor.row - 1) 0)

        cursor =
            editor.cursor
    in
        { editor | cursor = { cursor | row = row } }


motionWord : Editor -> Editor
motionWord editor =
    let
        row =
            editor.cursor.row

        line =
            (Maybe.withDefault "" (Array.get row editor.buffer))

        col =
            (nextWord line editor.cursor.col)

        cursor =
            editor.cursor
    in
        { editor | cursor = { cursor | col = col } }


motionWordBack : Editor -> Editor
motionWordBack editor =
    let
        row =
            editor.cursor.row

        line =
            (Maybe.withDefault "" (Array.get row editor.buffer))

        col =
            (prevWord line editor.cursor.col)

        cursor =
            editor.cursor
    in
        { editor | cursor = { cursor | col = col } }


nextWord : String -> Int -> Int
nextWord a i =
    wordIndexes a
        |> List.filter (\m -> m.index > i)
        |> List.map (\m -> m.index)
        |> List.head
        |> Maybe.withDefault 0


prevWord : String -> Int -> Int
prevWord a i =
    wordIndexes a
        |> List.reverse
        |> List.filter (\m -> m.index < i)
        |> List.map (\m -> m.index)
        |> List.head
        |> Maybe.withDefault 0


wordIndexes : String -> List Regex.Match
wordIndexes a =
    Regex.find Regex.All (Regex.regex "\\b\\w") a


deleteChar : Editor -> Editor
deleteChar editor =
    let
        row =
            editor.cursor.row

        col =
            editor.cursor.col + 1

        oldLine =
            Maybe.withDefault "" (Array.get row editor.buffer)

        newLine =
            (String.left (col - 1) oldLine) ++ (String.right ((String.length oldLine) - col) oldLine)

        buffer =
            (Array.set row newLine editor.buffer)

        editor =
            (left editor)
    in
        { editor | buffer = buffer }


insertChar : Keyboard.KeyCode -> Editor -> Editor
insertChar code editor =
    let
        row =
            editor.cursor.row

        col =
            editor.cursor.col - 1

        oldLine =
            Maybe.withDefault "" (Array.get row editor.buffer)

        newLine =
            (String.left (col) oldLine) ++ (fromCode code) ++ (String.right ((String.length oldLine) - col) oldLine)

        editor =
            (right editor)
    in
        { editor | buffer = (Array.set row newLine editor.buffer) }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Editor
            (Cursor 0 0)
            (Array.fromList [ "this is the buffer", "this is the second line", "this is the third line" ])
            40
            10
        )
        "this is the log"
        Normal
        False
        False
        False
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            let
                model =
                    newModifiers True code model

                log =
                    newLog code model.log
            in
                ( { model | log = log }, Cmd.none )

        KeyUp code ->
            let
                mode =
                    newMode code model.mode

                model =
                    newModifiers False code model
            in
                ( { model | mode = mode }, Cmd.none )


newModifiers : Bool -> Keyboard.KeyCode -> Model -> Model
newModifiers isDown code model =
    case code of
        16 ->
            { model | shift = isDown }

        17 ->
            { model | ctrl = isDown }

        18 ->
            { model | alt = isDown }

        _ ->
            if isDown then
                case model.mode of
                    Insert ->
                        case code of
                            -- esc
                            27 ->
                                { model | mode = Normal }

                            -- backspace
                            8 ->
                                { model | editor = deleteChar model.editor }

                            -- enter
                            -- 13 ->
                            -- { model | buffer = (model.editor.buffer ++ "\n") }
                            _ ->
                                { model | editor = insertChar code model.editor }

                    _ ->
                        case code of
                            -- a
                            65 ->
                                { model | mode = Insert, editor = (right model.editor) }

                            -- i
                            73 ->
                                { model | mode = Insert }

                            -- h
                            72 ->
                                { model | editor = (left model.editor) }

                            -- j
                            74 ->
                                { model | editor = (down model.editor) }

                            -- k
                            75 ->
                                { model | editor = (up model.editor) }

                            -- l
                            76 ->
                                { model | editor = (right model.editor) }

                            -- w
                            87 ->
                                { model | editor = (motionWord model.editor) }

                            -- b
                            66 ->
                                { model | editor = (motionWordBack model.editor) }

                            _ ->
                                model
            else
                model


newLog : Keyboard.KeyCode -> String -> String
newLog code log =
    "up: " ++ (toString code) ++ " - " ++ (fromCode code) ++ "\n" ++ log


newMode : Keyboard.KeyCode -> Mode -> Mode
newMode code mode =
    case code of
        _ ->
            mode


fromCode : Keyboard.KeyCode -> String
fromCode code =
    String.fromChar (Char.fromCode code)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", "20px" )
            , ( "top", "20px" )
            ]
        ]
        [ (renderCursor model)
        , (bufferPre model.editor)
        , pre [] [ text (statusBarText model) ]
        , pre [] [ text model.log ]
        ]


bufferPre : Editor -> Html Msg
bufferPre editor =
    pre
        [ style
            [ ( "background", "white" )
            , ( "width", (asPx (editor.width * 15)) )
            , ( "height", (asPx (editor.height * 15)) )
            ]
        ]
        [ text (Array.foldr joinArray "" editor.buffer) ]


joinArray : String -> String -> String
joinArray a b =
    a ++ "\n" ++ b


renderCursor : Model -> Html Msg
renderCursor model =
    div
        [ style
            [ ( "width"
              , cursorWidth model.editor.cursor model.mode
              )
            , ( "left", asPx (model.editor.cursor.col * 9) )
            , ( "height", "15px" )
            , ( "top", asPx (model.editor.cursor.row * 15) )
            , ( "position", "absolute" )
            , ( "background-color", "rgba(0,0,0,0.25)" )
            ]
        ]
        []


cursorWidth : Cursor -> Mode -> String
cursorWidth cursor mode =
    case mode of
        Insert ->
            "2px"

        _ ->
            "9px"


statusBarText : Model -> String
statusBarText model =
    "--" ++ (toString model.mode) ++ "-- , shift:" ++ (toString model.shift) ++ ", ctrl:" ++ (toString model.ctrl) ++ ", alt:" ++ (toString model.alt) ++ ", row:" ++ (toString model.editor.cursor.row) ++ ", col:" ++ (toString model.editor.cursor.col)


asPx : Int -> String
asPx x =
    (toString x) ++ "px"
