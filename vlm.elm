module Main exposing (..)

import Array
import Char
import Cursor exposing (..)
import Debug
import Html exposing (..)
import Html.App
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode)
import Regex
import String


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


cursorLeft : Editor -> Editor
cursorLeft editor =
    let
        col =
            (max (editor.cursor.col - 1) 0)
    in
        { editor | cursor = (withCol col editor.cursor) }


cursorRight : Editor -> Editor
cursorRight editor =
    let
        col =
            (min (editor.cursor.col + 1) (String.length (currentLine editor)))
    in
        { editor | cursor = (withCol col editor.cursor) }


cursorDown : Editor -> Editor
cursorDown editor =
    let
        row =
            (min (editor.cursor.row + 1) (editor.height - 1))
    in
        { editor | cursor = (withRow row editor.cursor) }


cursorUp : Editor -> Editor
cursorUp editor =
    let
        row =
            (max (editor.cursor.row - 1) 0)
    in
        { editor | cursor = (withRow row editor.cursor) }


cursorStart : Editor -> Editor
cursorStart e =
    { e | cursor = (withCol 0 e.cursor) }


cursorEnd : Editor -> Editor
cursorEnd e =
    { e | cursor = (withCol (String.length (currentLine e)) e.cursor) }


lineAt : Editor -> Int -> String
lineAt editor i =
    Maybe.withDefault "" (Array.get i editor.buffer)


replaceLineAt : Int -> String -> Editor -> Editor
replaceLineAt i l e =
    { e | buffer = (Array.set i l e.buffer) }


replaceCurrentLine : String -> Editor -> Editor
replaceCurrentLine l e =
    replaceLineAt e.cursor.row l e


currentLine : Editor -> String
currentLine editor =
    lineAt editor editor.cursor.row


prevLine : Editor -> String
prevLine editor =
    lineAt editor (editor.cursor.row - 1)


nextLine : Editor -> String
nextLine editor =
    lineAt editor (editor.cursor.row + 1)


wordIndexes : String -> List Regex.Match
wordIndexes a =
    Regex.find Regex.All (Regex.regex "\\b\\w") a


wordEndIndexes : String -> List Regex.Match
wordEndIndexes a =
    Regex.find Regex.All (Regex.regex "\\w\\b") a


nextIndex : Int -> List Regex.Match -> Maybe Int
nextIndex i list =
    list
        |> List.filter (\m -> m.index > i)
        |> List.map (\m -> m.index)
        |> List.head


prevIndex : Int -> List Regex.Match -> Maybe Int
prevIndex i list =
    list
        |> List.reverse
        |> List.filter (\m -> m.index < i)
        |> List.map (\m -> m.index)
        |> List.head


lastIndex : List Regex.Match -> Maybe Int
lastIndex list =
    list
        |> List.reverse
        |> List.map (\m -> m.index)
        |> List.head


motionWord : Editor -> Editor
motionWord e =
    case nextIndex e.cursor.col (wordIndexes (currentLine e)) of
        Just col ->
            { e | cursor = withCol col e.cursor }

        Nothing ->
            cursorStart (cursorDown e)


motionWordBack : Editor -> Editor
motionWordBack e =
    case prevIndex e.cursor.col (wordIndexes (currentLine e)) of
        Just col ->
            { e | cursor = withCol col e.cursor }

        Nothing ->
            case lastIndex (wordIndexes (prevLine e)) of
                Just col ->
                    cursorUp { e | cursor = withCol col e.cursor }

                Nothing ->
                    cursorUp e


motionWordEnd : Editor -> Editor
motionWordEnd e =
    case nextIndex e.cursor.col (wordEndIndexes (currentLine e)) of
        Just col ->
            { e | cursor = withCol col e.cursor }

        Nothing ->
            case nextIndex 0 (wordEndIndexes (nextLine e)) of
                Just col ->
                    cursorDown { e | cursor = withCol col e.cursor }

                Nothing ->
                    cursorDown e


splitAt : Int -> String -> ( String, String )
splitAt i a =
    ( String.left i a, String.right ((String.length a) - i) a )


deleteAt : Int -> String -> String
deleteAt i a =
    let
        split =
            splitAt i a
    in
        (String.dropRight 1 (fst split)) ++ snd split


deleteCharLeft : Editor -> Editor
deleteCharLeft e =
    e
        |> replaceCurrentLine (deleteAt e.cursor.col (currentLine e))
        |> cursorLeft


deleteCharRight : Editor -> Editor
deleteCharRight e =
    e
        |> replaceCurrentLine (deleteAt (e.cursor.col + 1) (currentLine e))
        |> cursorLeft


insertAt : Int -> String -> String -> String
insertAt i a b =
    let
        split =
            splitAt i b
    in
        fst split ++ a ++ snd split


insertChar : KeyCode -> Editor -> Editor
insertChar c e =
    e
        |> replaceCurrentLine (insertAt e.cursor.col (fromCode c) (currentLine e))
        |> cursorRight


insertLine : Editor -> Editor
insertLine e =
    let
        split =
            splitAt e.cursor.col (currentLine e)
    in
        e


bufLeft : Int -> Buffer -> Buffer
bufLeft i b =
    Array.slice 0 i b


bufRight : Int -> Buffer -> Buffer
bufRight i b =
    Array.slice i (Array.length b) b


bufInsertAt : Int -> String -> Buffer -> Buffer
bufInsertAt i s b =
    Array.append (Array.push s (bufLeft i b)) (bufRight i b)


insertLineAt : Int -> Editor -> Editor
insertLineAt i e =
    { e | buffer = bufInsertAt i "" e.buffer, cursor = Cursor i 0 }


insertLineBefore : Editor -> Editor
insertLineBefore e =
    insertLineAt e.cursor.row e


insertLineAfter : Editor -> Editor
insertLineAfter e =
    insertLineAt (e.cursor.row + 1) e


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
    = KeyDown KeyCode
    | KeyUp KeyCode
    | KeyPress KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            let
                model =
                    onKeyDown code model

                log =
                    newLog "down" code model.log
            in
                ( { model | log = log }, Cmd.none )

        KeyUp code ->
            ( onKeyUp code model, Cmd.none )

        KeyPress code ->
            let
                model =
                    onKeyPress code model

                log =
                    newLog "press" code model.log
            in
                ( { model | log = log }, Cmd.none )


onKeyUp : KeyCode -> Model -> Model
onKeyUp c m =
    case c of
        16 ->
            { m | shift = False }

        17 ->
            { m | ctrl = False }

        18 ->
            { m | alt = False }

        _ ->
            m


onKeyDown : KeyCode -> Model -> Model
onKeyDown c m =
    case c of
        16 ->
            { m | shift = True }

        17 ->
            { m | ctrl = True }

        18 ->
            { m | alt = True }

        _ ->
            case m.mode of
                Insert ->
                    case c of
                        -- esc
                        27 ->
                            { m | mode = Normal }

                        -- backspace
                        8 ->
                            { m | editor = deleteCharLeft m.editor }

                        -- delete
                        46 ->
                            { m | editor = deleteCharRight m.editor }

                        _ ->
                            m

                _ ->
                    m


onKeyPress : KeyCode -> Model -> Model
onKeyPress c m =
    case m.mode of
        Insert ->
            case c of
                -- enter
                -- 13 ->
                -- { m | buffer = (m.editor.buffer ++ "\n") }
                _ ->
                    { m | editor = insertChar c m.editor }

        _ ->
            case c of
                -- A
                65 ->
                    { m | mode = Insert, editor = cursorEnd m.editor }

                -- I
                73 ->
                    { m | mode = Insert, editor = cursorStart m.editor }

                -- O
                79 ->
                    { m | editor = insertLineBefore m.editor, mode = Insert }

                -- a
                97 ->
                    { m | mode = Insert, editor = cursorRight m.editor }

                -- b
                98 ->
                    { m | editor = motionWordBack m.editor }

                -- e
                101 ->
                    { m | editor = motionWordEnd m.editor }

                -- h
                104 ->
                    { m | editor = cursorLeft m.editor }

                -- i
                105 ->
                    { m | mode = Insert }

                -- j
                106 ->
                    { m | editor = cursorDown m.editor }

                -- k
                107 ->
                    { m | editor = cursorUp m.editor }

                -- l
                108 ->
                    { m | editor = cursorRight m.editor }

                -- o
                111 ->
                    { m | editor = insertLineAfter m.editor, mode = Insert }

                -- w
                119 ->
                    { m | editor = motionWord m.editor }

                -- x
                120 ->
                    { m
                        | editor =
                            m.editor
                                |> cursorRight
                                |> deleteCharLeft
                    }

                _ ->
                    m


newLog : String -> KeyCode -> String -> String
newLog a c log =
    a ++ ": " ++ (toString c) ++ " - " ++ (fromCode c) ++ "\n" ++ log


fromCode : KeyCode -> String
fromCode code =
    String.fromChar (Char.fromCode code)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        , Keyboard.presses KeyPress
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
            , ( "width", (asPx (editor.width * 9)) )
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
