module Model exposing (createRandomFood, init)

import Random
import Types exposing (Direction(..), Food, Model, Msg(..), TimerStatus(..), createNewFoodMsg)


randomFoodGenerator : Int -> Random.Generator Food
randomFoodGenerator halfSize =
    Random.pair
        (Random.int -halfSize halfSize)
        (Random.int -halfSize halfSize)


createRandomFood : Int -> Cmd Msg
createRandomFood halfSize =
    Random.generate createNewFoodMsg (randomFoodGenerator halfSize)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { halfSize = 5
      , snake = [ ( 0, 0 ) ]
      , food = ( 0, 0 )
      , currentDirection = None
      , timerStatus = Off
      , gameOver = False
      }
    , createRandomFood 5
    )
