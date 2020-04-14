module Types exposing (Cell, Direction(..), Food, Model, Msg(..), Snake, TimerStatus(..), createKeyMsg, createNewFoodMsg, createTimerMsg)

import Time exposing (Posix)


type alias Model =
    { halfSize : Int
    , snake : Snake
    , food : Food
    , currentDirection : Direction
    , timerStatus : TimerStatus
    }


type Direction
    = Up
    | Down
    | Left
    | Right
    | None


type Msg
    = Ignore
    | NewFood Food
    | MovementKey Direction
    | StartMovementTimer
    | MovementTimer Direction Posix


type TimerStatus
    = Off
    | On


createNewFoodMsg : Food -> Msg
createNewFoodMsg food =
    NewFood food


createKeyMsg : String -> Msg
createKeyMsg key =
    case key of
        "ArrowDown" ->
            MovementKey Down

        "ArrowUp" ->
            MovementKey Up

        "ArrowLeft" ->
            MovementKey Left

        "ArrowRight" ->
            MovementKey Right

        _ ->
            Ignore


createTimerMsg : Direction -> Posix -> Msg
createTimerMsg direction posix =
    MovementTimer direction posix


type alias Cell =
    ( Int, Int )


type alias Snake =
    List Cell


type alias Food =
    Cell
