module Utils exposing (isPartOfSnake, cellsAreEqual, moveSnake, isValidDirection)

import Types exposing (Cell, Snake, Direction(..))


isPartOfSnake : Cell -> Snake -> Bool
isPartOfSnake cell snake =
    case snake of
        [] ->
            False

        head :: tail ->
            cellsAreEqual cell head || isPartOfSnake cell tail


cellsAreEqual : Cell -> Cell -> Bool
cellsAreEqual ( row1, col1 ) ( row2, col2 ) =
    row1 == row2 && col1 == col2

moveSnake : Snake -> Direction -> Snake
moveSnake snake direction =
    case snake of
        [] -> []
        head::_ ->
            let 
                newHead = calculateNewHead head direction
            in
                newHead::dropLast snake

calculateNewHead : Cell -> Direction -> Cell
calculateNewHead (row, col) direction =
    case direction of
        Left -> (row, col-1)
        Right -> (row, col+1)
        Up -> (row-1, col)
        Down -> (row+1, col)
        _ -> (row, col)

dropLast : List a -> List a
dropLast list =
    case list of
        [] -> []
        [_] -> []
        _ -> List.take 
                (List.length list-1)
                list

isValidDirection : Direction -> Direction -> Bool
isValidDirection newDirection currentDirection =
    case currentDirection of
        None -> True
        Up -> newDirection == Left || newDirection == Right
        Down -> newDirection == Left || newDirection == Right
        Left -> newDirection == Up || newDirection == Down
        Right -> newDirection == Up || newDirection == Down
