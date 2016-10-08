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


type alias Model =
    { cursor : Cursor
    , buffer : Buffer
    , width : Int
    , height : Int
    , log : String
    , mode : Mode
    , ctrl : Bool
    , shift : Bool
    , alt : Bool
    }


cursorLeft : Model -> Model
cursorLeft m =
    let
        col =
            (max (m.cursor.col - 1) 0)
    in
        { m | cursor = (Cursor.withCol col m.cursor) }


cursorRight : Model -> Model
cursorRight m =
    let
        col =
            (min (m.cursor.col + 1) (String.length (currentLine m)))
    in
        { m | cursor = (Cursor.withCol col m.cursor) }


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


replaceLineAt : Int -> String -> Model -> Model
replaceLineAt i s m =
    { m | buffer = (Buffer.set i s m.buffer) }


replaceCurrentLine : String -> Model -> Model
replaceCurrentLine s m =
    replaceLineAt m.cursor.row s m


currentLine : Model -> String
currentLine m =
    Buffer.get m.cursor.row m.buffer


prevLine : Model -> String
prevLine m =
    Buffer.get (m.cursor.row - 1) m.buffer


nextLine : Model -> String
nextLine m =
    Buffer.get (m.cursor.row + 1) m.buffer


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


deleteCharLeft : Model -> Model
deleteCharLeft m =
    if m.cursor.col == 0 && m.cursor.row /= 0 then
        joinLines m
    else
        replaceCurrentLine (deleteAt m.cursor.col (currentLine m)) m


deleteCharRight : Model -> Model
deleteCharRight m =
    replaceCurrentLine (deleteAt (m.cursor.col + 1) (currentLine m)) m


insertAt : Int -> String -> String -> String
insertAt i a b =
    let
        split =
            splitAt i b
    in
        fst split ++ a ++ snd split


insertChar : KeyCode -> Model -> Model
insertChar c m =
    m
        |> replaceCurrentLine (insertAt m.cursor.col (fromCode c) (currentLine m))
        |> cursorRight


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
            splitAt m.cursor.col (currentLine m)
    in
        m
            |> replaceCurrentLine (fst split)
            |> insertLineAt (snd split) (m.cursor.row + 1)
            |> cursorStart


insertLineBefore : Model -> Model
insertLineBefore m =
    insertEmptyLineAt m.cursor.row m


insertLineAfter : Model -> Model
insertLineAfter m =
    insertEmptyLineAt (m.cursor.row + 1) m


startSelection : Model -> Model
startSelection m =
    { m | buffer = (Buffer.select m.cursor.row (Buffer.Selection m.cursor.col (m.cursor.col + 1)) m.buffer) }


startVisualMode : Model -> Model
startVisualMode m =
    startSelection { m | mode = Visual }


startInsertMode : Model -> Model
startInsertMode m =
    { m | mode = Insert }


motionLeft : Model -> Model
motionLeft m =
    cursorLeft m


motionRight : Model -> Model
motionRight m =
    cursorRight m


init : ( Model, Cmd Msg )
init =
    ( Model
        (Cursor 0 0)
        (Array.fromList [ ( "this is the buffer", Nothing ), ( "this is the second line", Nothing ), ( "this is the third line", Nothing ) ])
        80
        10
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
            cursorUp m

        39 ->
            motionRight m

        40 ->
            cursorDown m

        _ ->
            case m.mode of
                Insert ->
                    case c of
                        -- esc
                        27 ->
                            { m | mode = Normal }

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

                -- O
                79 ->
                    insertLineBefore { m | mode = Insert }

                -- a
                97 ->
                    startInsertMode (motionRight m)

                -- b
                98 ->
                    motionWordBack m

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
        , pre [] [ text (statusBarText m) ]
        , pre [] [ text m.log ]
        ]


renderBuffer : Model -> Html Msg
renderBuffer m =
    div
        [ (class "buffer")
        , style
            [ ( "width", asPx (m.width * 9) )
            , ( "height", asPx (m.height * 15) )
            ]
        ]
        (Array.toList (Array.indexedMap lineMapper m.buffer))


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
            , ( "left", asPx (m.cursor.col * 9) )
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
            "9px"


statusBarText : Model -> String
statusBarText m =
    "--" ++ (toString m.mode) ++ "-- , shift:" ++ (toString m.shift) ++ ", ctrl:" ++ (toString m.ctrl) ++ ", alt:" ++ (toString m.alt) ++ ", row:" ++ (toString m.cursor.row) ++ ", col:" ++ (toString m.cursor.col)


asPx : Int -> String
asPx x =
    (toString x) ++ "px"
