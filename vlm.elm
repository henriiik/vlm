module Main exposing (..)

import Array
import Char
import Cursor exposing (Cursor)
import Buffer exposing (Buffer)
import Selection exposing (Selection)
import Register exposing (Register)
import Line exposing (Line)
import Html exposing (..)
import Html
import Html.Attributes exposing (style, class)
import Keyboard exposing (KeyCode)
import Regex
import String


main : Program Never Model Msg
main =
    Html.program
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


type alias Model =
    { cursor : Cursor
    , selectionStart : Cursor
    , buffer : Buffer
    , register : Register
    , width : Int
    , height : Int
    , log : List String
    , mode : Mode
    , ctrl : Bool
    , shift : Bool
    , alt : Bool
    }


cursorDown : Model -> Model
cursorDown m =
    let
        row =
            (min (m.cursor.row + 1) (m.height - 1))
    in
        { m | cursor = (Cursor.withRow row m.cursor) }


cursorUp : Model -> Model
cursorUp m =
    let
        row =
            (max (m.cursor.row - 1) 0)
    in
        { m | cursor = (Cursor.withRow row m.cursor) }


cursorStart : Model -> Model
cursorStart m =
    { m | cursor = (Cursor.withCol 0 m.cursor) }


cursorEnd : Model -> Model
cursorEnd m =
    { m | cursor = (Cursor.withCol (String.length (currentLine m)) m.cursor) }


currentSelection : Model -> Selection
currentSelection m =
    Selection.fromCursors m.cursor m.selectionStart


replaceLineAt : Int -> Line -> Model -> Model
replaceLineAt i s m =
    { m | buffer = (Buffer.set i s m.buffer) }


replaceCurrentLine : Line -> Model -> Model
replaceCurrentLine s m =
    replaceLineAt m.cursor.row s m


currentLine : Model -> Line
currentLine m =
    Buffer.get m.cursor.row m.buffer


currentLineLength : Model -> Int
currentLineLength m =
    m
        |> currentLine
        |> String.length


prevLine : Model -> Line
prevLine m =
    Buffer.get (m.cursor.row - 1) m.buffer


nextLine : Model -> Line
nextLine m =
    Buffer.get (m.cursor.row + 1) m.buffer


wordIndexes : Line -> List Regex.Match
wordIndexes a =
    Regex.find Regex.All (Regex.regex "\\b\\w") a


wordEndIndexes : Line -> List Regex.Match
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


motionWord : Model -> Model
motionWord m =
    case nextIndex m.cursor.col (wordIndexes (currentLine m)) of
        Just col ->
            { m | cursor = Cursor.withCol col m.cursor }

        Nothing ->
            cursorStart (cursorDown m)


motionWordBack : Model -> Model
motionWordBack m =
    case prevIndex m.cursor.col (wordIndexes (currentLine m)) of
        Just col ->
            { m | cursor = Cursor.withCol col m.cursor }

        Nothing ->
            case lastIndex (wordIndexes (prevLine m)) of
                Just col ->
                    cursorUp { m | cursor = Cursor.withCol col m.cursor }

                Nothing ->
                    cursorUp m


motionWordEnd : Model -> Model
motionWordEnd m =
    case nextIndex m.cursor.col (wordEndIndexes (currentLine m)) of
        Just col ->
            { m | cursor = Cursor.withCol col m.cursor }

        Nothing ->
            case nextIndex 0 (wordEndIndexes (nextLine m)) of
                Just col ->
                    cursorDown { m | cursor = Cursor.withCol col m.cursor }

                Nothing ->
                    cursorDown m


deleteCharLeft : Model -> Model
deleteCharLeft m =
    if m.cursor.col == 0 && m.cursor.row /= 0 then
        joinLines m
    else
        replaceCurrentLine (Line.deleteChar m.cursor.col (currentLine m)) m


deleteCharRight : Model -> Model
deleteCharRight m =
    replaceCurrentLine (Line.deleteChar (m.cursor.col + 1) (currentLine m)) m


insertChar : KeyCode -> Model -> Model
insertChar c m =
    m
        |> replaceCurrentLine (Line.insert m.cursor.col (fromCode c) (currentLine m))
        |> motionRight


insertLineAt : String -> Int -> Model -> Model
insertLineAt s i m =
    { m | buffer = Buffer.insert i s m.buffer, cursor = Cursor i 0 }


removeLineAt : Int -> Model -> Model
removeLineAt i m =
    { m | buffer = Buffer.remove i m.buffer }


insertEmptyLineAt : Int -> Model -> Model
insertEmptyLineAt i m =
    insertLineAt "" i m


joinLines : Model -> Model
joinLines m =
    let
        line =
            (prevLine m) ++ (currentLine m)
    in
        m
            |> cursorUp
            |> cursorEnd
            |> replaceCurrentLine line


splitLine : Model -> Model
splitLine m =
    let
        split =
            Line.split m.cursor.col (currentLine m)
    in
        m
            |> replaceCurrentLine (Tuple.first split)
            |> insertLineAt (Tuple.second split) (m.cursor.row + 1)
            |> cursorStart


insertLineBefore : Model -> Model
insertLineBefore m =
    insertEmptyLineAt m.cursor.row m


insertLineAfter : Model -> Model
insertLineAfter m =
    insertEmptyLineAt (m.cursor.row + 1) m


startSelection : Model -> Model
startSelection m =
    { m | selectionStart = m.cursor }


cutSelection : Mode -> Selection -> Buffer -> ( Buffer, Register )
cutSelection mode sel buf =
    case mode of
        VisualLine ->
            Register.cutLines sel buf

        _ ->
            Register.cut sel buf


deleteSelection : Model -> Model
deleteSelection m =
    let
        sel =
            currentSelection m

        ( buf, reg ) =
            cutSelection m.mode sel m.buffer
    in
        { m
            | mode = Normal
            , cursor = sel.start
            , buffer = buf
            , register = reg
        }


pasteBefore : Model -> Model
pasteBefore m =
    let
        ( buf, cur ) =
            Register.insert m.register m.cursor m.buffer
    in
        { m
            | buffer = buf
            , cursor = cur
        }


pasteAfter : Model -> Model
pasteAfter m =
    case m.register of
        Register.Normal _ ->
            pasteBefore (motionRight m)

        Register.Line _ ->
            pasteBefore (cursorDown m)


startVisualMode : Model -> Model
startVisualMode m =
    startSelection { m | mode = Visual }


startVisualLineMode : Model -> Model
startVisualLineMode m =
    startSelection { m | mode = VisualLine }


startInsertMode : Model -> Model
startInsertMode m =
    { m | mode = Insert }


motionLeft : Model -> Model
motionLeft m =
    if m.cursor.col == 0 then
        m
    else
        { m | cursor = Cursor.left m.cursor }


motionRight : Model -> Model
motionRight m =
    if m.cursor.col >= currentLineLength m then
        m
    else
        { m | cursor = Cursor.right m.cursor }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Cursor 0 0)
        (Cursor 0 0)
        (Array.fromList [ "this is the buffer", "this is the second line", "this is the third line", "this is the fourth line", "this is the fifth line", "this is the sixth line", "this is the seventh", "this is the eighth line", "this is the ninth line" ])
        (Register.Line (Array.fromList [ "paste!" ]))
        80
        20
        [ "this is the log" ]
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
                newModel =
                    onKeyDown code model

                log =
                    newLog "down" code model.log
            in
                ( { newModel | log = log }, Cmd.none )

        KeyUp code ->
            ( onKeyUp code model, Cmd.none )

        KeyPress code ->
            let
                newModel =
                    onKeyPress code model

                log =
                    newLog "press" code model.log
            in
                ( { newModel | log = log }, Cmd.none )


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
            cursorUp m

        39 ->
            motionRight m

        40 ->
            cursorDown m

        -- esc
        27 ->
            { m | mode = Normal }

        _ ->
            case m.mode of
                Insert ->
                    case c of
                        -- backspace
                        8 ->
                            motionLeft (deleteCharLeft m)

                        -- delete
                        46 ->
                            deleteCharRight m

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
                    splitLine m

                _ ->
                    insertChar c m

        _ ->
            case c of
                -- A
                65 ->
                    cursorEnd { m | mode = Insert }

                -- I
                73 ->
                    cursorStart { m | mode = Insert }

                -- V
                86 ->
                    startVisualLineMode m

                -- O
                79 ->
                    insertLineBefore { m | mode = Insert }

                -- P
                80 ->
                    pasteBefore m

                -- a
                97 ->
                    startInsertMode (motionRight m)

                -- b
                98 ->
                    motionWordBack m

                -- d
                100 ->
                    deleteSelection m

                -- e
                101 ->
                    motionWordEnd m

                -- h
                104 ->
                    motionLeft m

                -- i
                105 ->
                    { m | mode = Insert }

                -- j
                106 ->
                    cursorDown m

                -- k
                107 ->
                    cursorUp m

                -- l
                108 ->
                    motionRight m

                -- p
                112 ->
                    pasteAfter m

                -- o
                111 ->
                    insertLineAfter { m | mode = Insert }

                -- v
                118 ->
                    startVisualMode m

                -- w
                119 ->
                    motionWord m

                -- x
                120 ->
                    deleteCharRight m

                _ ->
                    m


newLog : String -> KeyCode -> List String -> List String
newLog a c log =
    log
        |> List.append [ a ++ ": " ++ (toString c) ++ " - '" ++ (fromCode c) ++ "'" ]
        |> List.take 10


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
view m =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", "20px" )
            , ( "top", "20px" )
            ]
        ]
        [ (renderCursor m)
        , (renderBuffer m)
        , div [ (class "status") ] [ text (statusBarText m) ]
        , div [ (class "log") ] [ text (renderLog m.log) ]
        ]


renderLog : List String -> String
renderLog log =
    log
        |> List.map (\a -> a ++ "\n")
        |> List.foldr (\a b -> a ++ b) ""


renderBuffer : Model -> Html Msg
renderBuffer m =
    let
        s =
            currentSelection m
    in
        div
            [ (class "buffer")
            , style
                [ ( "width", asCh m.width )
                , ( "height", asPx (m.height * 15) )
                ]
            ]
            (m.buffer
                |> Array.slice 0 m.height
                |> Array.indexedMap (renderLine m.mode s)
                |> Array.toList
            )


renderLine : Mode -> Selection -> Int -> Line -> Html Msg
renderLine m s row l =
    div
        [ (class "line")
        , style [ ( "top", asPx (row * 15) ) ]
        ]
        [ text l
        , (renderSelection m s row l)
        ]


renderSelectionStart : Mode -> Cursor -> Int -> Int
renderSelectionStart m c row =
    case m of
        VisualLine ->
            0

        Visual ->
            if c.row < row then
                0
            else
                c.col

        _ ->
            0


renderSelectionEnd : Mode -> Cursor -> Int -> Line -> Int
renderSelectionEnd m c row l =
    case m of
        VisualLine ->
            String.length l

        Visual ->
            if c.row > row then
                String.length l
            else
                c.col

        _ ->
            0


renderSelection : Mode -> Selection -> Int -> Line -> Html Msg
renderSelection m s row l =
    if s.start.row > row || s.end.row < row then
        text ""
    else
        let
            start =
                renderSelectionStart m s.start row

            width =
                (renderSelectionEnd m s.end row l) - start
        in
            div
                [ class "selection"
                , style
                    [ ( "left", asCh start )
                    , ( "width", asCh width )
                    ]
                ]
                []


joinArray : String -> String -> String
joinArray a b =
    a ++ "\n" ++ b


renderCursor : Model -> Html Msg
renderCursor m =
    div
        [ (class "cursor")
        , style
            [ ( "width", cursorWidth m )
            , ( "left", asCh m.cursor.col )
            , ( "top", asPx (m.cursor.row * 15) )
            ]
        ]
        []


cursorWidth : Model -> String
cursorWidth m =
    case m.mode of
        Insert ->
            "2px"

        _ ->
            "1ch"


statusBarText : Model -> String
statusBarText m =
    "-- "
        ++ (toString m.mode)
        ++ " -- , shift:"
        ++ (toString m.shift)
        ++ ", ctrl:"
        ++ (toString m.ctrl)
        ++ ", alt:"
        ++ (toString m.alt)
        ++ (cursorStatus ", cursor " m.cursor)
        ++ (cursorStatus ", selection " m.selectionStart)


cursorStatus : String -> Cursor -> String
cursorStatus s c =
    s ++ (toString c.row) ++ ":" ++ (toString c.col)


asPx : Int -> String
asPx x =
    (toString x) ++ "px"


asCh : Int -> String
asCh x =
    (toString x) ++ "ch"
