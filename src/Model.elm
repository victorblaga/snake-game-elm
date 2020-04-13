module Model exposing (init, createRandomFood)

import Random
import Types exposing (Food, Model, Msg(..), createNewFoodMsg)


randomFoodGenerator : Int -> Random.Generator Food
randomFoodGenerator halfSize =
    Random.pair
        (Random.int -halfSize halfSize)
        (Random.int -halfSize halfSize)

createRandomFood : Int -> Cmd Msg
createRandomFood halfSize
    = Random.generate createNewFoodMsg (randomFoodGenerator halfSize)

init : () -> ( Model, Cmd Msg )
init _ =
    ( { halfSize = 5
      , snake = [ ( 0, 0 ) ]
      , food = (0, 0)
      }
    , createRandomFood 5
    )
