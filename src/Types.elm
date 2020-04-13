module Types exposing (Cell, Snake, Model, Msg(..), Food, createNewFoodMsg)

type alias Model =
    { halfSize : Int
    , snake : Snake
    , food : Food }


type Msg
    = Init
    | NewFood Food

createNewFoodMsg : Food -> Msg
createNewFoodMsg food = NewFood food

type alias Cell = (Int, Int)
type alias Snake = List Cell

type alias Food = Cell
