module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode exposing (Decoder, field, string)
import Model exposing (createRandomFood, init)
import Time
import Types exposing (Direction(..), Model, Msg(..), TimerStatus(..), createKeyMsg, createTimerMsg)
import Utils exposing (isPartOfSnake, isValidDirection, moveSnake)
import View exposing (view)
import Task


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions { currentDirection, timerStatus } =
    Sub.batch
        [ onKeyDown keyHandler
        , timer timerStatus currentDirection
        ]


tickMillis : Float
tickMillis =
    1000.0


timer : TimerStatus -> Direction -> Sub Msg
timer timerStatus currentDirection =
    case timerStatus of
        Off ->
            Sub.none

        On ->
            Time.every tickMillis (createTimerMsg currentDirection)


keyHandler : Decoder Msg
keyHandler =
    Decode.map createKeyMsg (field "key" string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ halfSize, snake } as model) =
    case msg of
        NewFood newFood ->
            if isPartOfSnake newFood snake then
                ( model, createRandomFood halfSize )

            else
                ( { model | food = newFood }, Cmd.none )

        MovementKey newDirection ->
            onMovementKey model newDirection

        MovementTimer newDirection _ ->
            onMovementTimer model newDirection

        StartMovementTimer ->
            ( { model | timerStatus = On }, Cmd.none )

        _ ->
            ( model, Cmd.none )


onMovementKey : Model -> Direction -> ( Model, Cmd Msg )
onMovementKey ({ snake, currentDirection } as model) newDirection =
    if isValidDirection newDirection currentDirection then
        let
            newSnake =
                moveSnake snake newDirection
        in
        ( { model
        | snake = newSnake
        , currentDirection = newDirection
        , timerStatus = Off
        }
    , Task.perform identity (Task.succeed StartMovementTimer)
    )

    else
        ( model, Cmd.none )

onMovementTimer : Model -> Direction -> ( Model, Cmd Msg )
onMovementTimer ({ snake } as model) newDirection =
    let
        newSnake =
            moveSnake snake newDirection
    in
    ( { model | snake = newSnake }
    , Cmd.none
    )