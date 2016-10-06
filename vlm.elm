module Main exposing (..)

import Array
import Char
import Cursor exposing (Cursor)
import Buffer exposing (Buffer)
import Html exposing (..)
import Html.App
import Html.Attributes exposing (style, class)
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
    | Visual
    | VisualLine


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
        { editor | cursor = (Cursor.withCol col editor.cursor) }


cursorRight : Editor -> Editor
cursorRight editor =
    let
        col =
            (min (editor.cursor.col + 1) (String.length (currentLine editor)))
    in
        { editor | cursor = (Cursor.withCol col editor.cursor) }


cursorDown : Editor -> Editor
cursorDown editor =
    let
        row =
            (min (editor.cursor.row + 1) (editor.height - 1))
    in
        { editor | cursor = (Cursor.withRow row editor.cursor) }


cursorUp : Editor -> Editor
cursorUp editor =
    let
        row =
            (max (editor.cursor.row - 1) 0)
    in
        { editor | cursor = (Cursor.withRow row editor.cursor) }


cursorStart : Editor -> Editor
cursorStart e =
    { e | cursor = (Cursor.withCol 0 e.cursor) }


cursorEnd : Editor -> Editor
cursorEnd e =
    { e | cursor = (Cursor.withCol (String.length (currentLine e)) e.cursor) }


replaceLineAt : Int -> String -> Editor -> Editor
replaceLineAt i s e =
    { e | buffer = (Buffer.set i s e.buffer) }


replaceCurrentLine : String -> Editor -> Editor
replaceCurrentLine s e =
    replaceLineAt e.cursor.row s e


currentLine : Editor -> String
currentLine e =
    Buffer.get e.cursor.row e.buffer


prevLine : Editor -> String
prevLine e =
    Buffer.get (e.cursor.row - 1) e.buffer


nextLine : Editor -> String
nextLine e =
    Buffer.get (e.cursor.row + 1) e.buffer


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
            { e | cursor = Cursor.withCol col e.cursor }

        Nothing ->
            cursorStart (cursorDown e)


motionWordBack : Editor -> Editor
motionWordBack e =
    case prevIndex e.cursor.col (wordIndexes (currentLine e)) of
        Just col ->
            { e | cursor = Cursor.withCol col e.cursor }

        Nothing ->
            case lastIndex (wordIndexes (prevLine e)) of
                Just col ->
                    cursorUp { e | cursor = Cursor.withCol col e.cursor }

                Nothing ->
                    cursorUp e


motionWordEnd : Editor -> Editor
motionWordEnd e =
    case nextIndex e.cursor.col (wordEndIndexes (currentLine e)) of
        Just col ->
            { e | cursor = Cursor.withCol col e.cursor }

        Nothing ->
            case nextIndex 0 (wordEndIndexes (nextLine e)) of
                Just col ->
                    cursorDown { e | cursor = Cursor.withCol col e.cursor }

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
    if e.cursor.col == 0 && e.cursor.row /= 0 then
        joinLines e
    else
        replaceCurrentLine (deleteAt e.cursor.col (currentLine e)) e


deleteCharRight : Editor -> Editor
deleteCharRight e =
    replaceCurrentLine (deleteAt (e.cursor.col + 1) (currentLine e)) e


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


insertLineAt : String -> Int -> Editor -> Editor
insertLineAt s i e =
    { e | buffer = Buffer.insert i s e.buffer, cursor = Cursor i 0 }


removeLineAt : Int -> Editor -> Editor
removeLineAt i e =
    { e | buffer = Buffer.remove i e.buffer }


insertEmptyLineAt : Int -> Editor -> Editor
insertEmptyLineAt i e =
    insertLineAt "" i e


joinLines : Editor -> Editor
joinLines e =
    let
        line =
            (prevLine e) ++ (currentLine e)
    in
        e
            |> cursorUp
            |> cursorEnd
            |> replaceCurrentLine line


splitLine : Editor -> Editor
splitLine e =
    let
        split =
            splitAt e.cursor.col (currentLine e)
    in
        e
            |> replaceCurrentLine (fst split)
            |> insertLineAt (snd split) (e.cursor.row + 1)
            |> cursorStart


insertLineBefore : Editor -> Editor
insertLineBefore e =
    insertEmptyLineAt e.cursor.row e


insertLineAfter : Editor -> Editor
insertLineAfter e =
    insertEmptyLineAt (e.cursor.row + 1) e


startSelection : Editor -> Editor
startSelection e =
    { e | buffer = (Buffer.select e.cursor.row (Buffer.Selection e.cursor.col (e.cursor.col + 1)) e.buffer) }


startVisualMode : Model -> Model
startVisualMode m =
    { m | mode = Visual, editor = startSelection m.editor }


startInsertMode : Model -> Model
startInsertMode m =
    { m | mode = Insert }


motionLeft : Model -> Model
motionLeft m =
    { m | editor = cursorLeft m.editor }


motionRight : Model -> Model
motionRight m =
    { m | editor = cursorRight m.editor }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Editor
            (Cursor 0 0)
            (Array.fromList [ ( "this is the buffer", Nothing ), ( "this is the second line", Nothing ), ( "this is the third line", Nothing ) ])
            80
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

        37 ->
            motionLeft m

        38 ->
            { m | editor = cursorUp m.editor }

        39 ->
            motionRight m

        40 ->
            { m | editor = cursorDown m.editor }

        _ ->
            case m.mode of
                Insert ->
                    case c of
                        -- esc
                        27 ->
                            { m | mode = Normal }

                        -- backspace
                        8 ->
                            motionLeft { m | editor = deleteCharLeft m.editor }

                        -- delete
                        46 ->
                            { m | editor = deleteCharRight m.editor }

                        _ ->
                            m

                Visual ->
                    case c of
                        -- esc
                        27 ->
                            { m | mode = Normal }

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
                13 ->
                    { m | editor = splitLine m.editor }

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
                    startInsertMode (motionRight m)

                -- b
                98 ->
                    { m | editor = motionWordBack m.editor }

                -- e
                101 ->
                    { m | editor = motionWordEnd m.editor }

                -- h
                104 ->
                    motionLeft m

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
                    motionRight m

                -- o
                111 ->
                    { m | editor = insertLineAfter m.editor, mode = Insert }

                -- v
                118 ->
                    startVisualMode m

                -- w
                119 ->
                    { m | editor = motionWord m.editor }

                -- x
                120 ->
                    { m | editor = deleteCharRight m.editor }

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
        , (renderBuffer model.editor)
        , pre [] [ text (statusBarText model) ]
        , pre [] [ text model.log ]
        ]


renderBuffer : Editor -> Html Msg
renderBuffer e =
    div
        [ (class "buffer")
        , style
            [ ( "width", asPx (e.width * 9) )
            , ( "height", asPx (e.height * 15) )
            ]
        ]
        (Array.toList (Array.indexedMap lineMapper e.buffer))


lineMapper : Int -> Buffer.Line -> Html Msg
lineMapper i l =
    div
        [ (class "line")
        , style [ ( "top", asPx (i * 15) ) ]
        ]
        [ text (fst l)
        , (renderSelection l)
        ]


renderSelection : Buffer.Line -> Html Msg
renderSelection l =
    case snd l of
        Just s ->
            div
                [ class "selection"
                , style
                    [ ( "left", asPx (s.start * 9) )
                    , ( "width", asPx ((s.end - s.start) * 9) )
                    ]
                ]
                []

        _ ->
            div [] []


joinArray : String -> String -> String
joinArray a b =
    a ++ "\n" ++ b


renderCursor : Model -> Html Msg
renderCursor m =
    div
        [ (class "cursor")
        , style
            [ ( "width", cursorWidth m )
            , ( "left", asPx (m.editor.cursor.col * 9) )
            , ( "top", asPx (m.editor.cursor.row * 15) )
            ]
        ]
        []


cursorWidth : Model -> String
cursorWidth m =
    case m.mode of
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
