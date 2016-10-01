module Main exposing (..)

import Char
import Html exposing (..)
import Html.App
import Html.Attributes exposing (style)
import Keyboard
import String
import Array


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


left : Cursor -> Cursor
left cursor =
    { cursor | col = (max (cursor.col - 1) 0) }


right : Cursor -> Cursor
right cursor =
    { cursor | col = cursor.col + 1 }


down : Cursor -> Cursor
down cursor =
    { cursor | row = cursor.row + 1 }


up : Cursor -> Cursor
up cursor =
    { cursor | row = (max (cursor.row - 1) 0) }


deleteChar : Model -> Model
deleteChar model =
    let
        oldLine =
            Maybe.withDefault "" (Array.get model.cursor.row model.buffer)

        newLine =
            (String.left (model.cursor.col - 1) oldLine) ++ (String.right ((String.length oldLine) - model.cursor.col) oldLine)
    in
        { model | buffer = (Array.set model.cursor.row newLine model.buffer), cursor = (left model.cursor) }


insertChar : Keyboard.KeyCode -> Model -> Model
insertChar code model =
    let
        oldLine =
            Maybe.withDefault "" (Array.get model.cursor.row model.buffer)

        newLine =
            (String.left (model.cursor.col) oldLine) ++ (fromCode code) ++ (String.right ((String.length oldLine) - model.cursor.col) oldLine)
    in
        { model | buffer = (Array.set model.cursor.row newLine model.buffer), cursor = (right model.cursor) }

type alias Buffer =
    Array.Array String


type alias Editor =
    { cursor : Cursor
    , buffer : Buffer
    , width : Int
    , height : Int
    }

type alias Model =
    { buffer : Array.Array String
    , cursor : Cursor
    , log : String
    , mode : Mode
    , ctrl : Bool
    , shift : Bool
    , alt : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Array.fromList [ "this is the buffer", "this is the second line", "this is the third line" ]) (Cursor 0 0) "this is the log" Normal False False False, Cmd.none )



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
                                deleteChar model

                            -- enter
                            -- 13 ->
                            -- { model | buffer = (model.buffer ++ "\n") }
                            _ ->
                                insertChar code model

                    _ ->
                        case code of
                            -- a
                            65 ->
                                { model | mode = Insert, cursor = (right model.cursor) }

                            -- i
                            73 ->
                                { model | mode = Insert }

                            -- h
                            72 ->
                                { model | cursor = (left model.cursor) }

                            -- j
                            74 ->
                                { model | cursor = (down model.cursor) }

                            -- k
                            75 ->
                                { model | cursor = (up model.cursor) }

                            -- l
                            76 ->
                                { model | cursor = (right model.cursor) }

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
        , (bufferPre model)
        , div [] [ text (statusBarText model) ]
        , pre [] [ text model.log ]
        ]


bufferPre : Model -> Html Msg
bufferPre model =
    pre
        [ style
            [ ( "border", "solid 1px black" )
            , ( "font-size", "15px" )
            , ( "line-height", "15px" )
            , ( "margin", "0px" )
            ]
        ]
        [ text (Array.foldr joinArray "" model.buffer) ]


joinArray : String -> String -> String
joinArray a b =
    a ++ "\n" ++ b


renderCursor : Model -> Html Msg
renderCursor model =
    div
        [ style
            [ ( "width"
              , cursorWidth model.cursor model.mode
              )
            , ( "left", (toString (model.cursor.col * 9)) ++ "px" )
            , ( "height", "15px" )
            , ( "top", (toString (model.cursor.row * 15)) ++ "px" )
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
    "mode:" ++ (toString model.mode) ++ ", shift:" ++ (toString model.shift) ++ ", ctrl:" ++ (toString model.ctrl) ++ ", alt:" ++ (toString model.alt) ++ ", row:" ++ (toString model.cursor.row) ++ ", col:" ++ (toString model.cursor.col)
