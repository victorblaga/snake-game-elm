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
subscriptions { currentDirection, timerStatus, gameOver } =
    Sub.batch
        [ onKeyDown keyHandler
        , timer gameOver timerStatus currentDirection
        ]


tickMillis : Float
tickMillis =
    1000.0


timer : Bool -> TimerStatus -> Direction -> Sub Msg
timer gameOver timerStatus currentDirection =
    if gameOver then
        Sub.none
    else
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
onMovementKey ({ currentDirection, halfSize } as model) newDirection =
    if isValidDirection newDirection currentDirection then
        let
            (newSnake, willEat, gameOver) =
                moveSnake model newDirection
        in
        ( { model
        | snake = newSnake
        , currentDirection = newDirection
        , timerStatus = Off
        , gameOver = gameOver
        }
    , Cmd.batch
        [ timerCmd
        , foodCmd willEat halfSize ]
    )

    else
        ( model, Cmd.none )

onMovementTimer : Model -> Direction -> ( Model, Cmd Msg )
onMovementTimer ({ halfSize } as model) newDirection =
    let
        (newSnake, willEat, gameOver) =
            moveSnake model newDirection
    in
    ( { model | snake = newSnake, gameOver = gameOver }
    , foodCmd willEat halfSize
    )

timerCmd : Cmd Msg
timerCmd = Task.perform identity (Task.succeed StartMovementTimer)

foodCmd : Bool -> Int -> Cmd Msg
foodCmd willEat halfSize =
    if willEat then
        createRandomFood halfSize
    else
        Cmd.none