module Utils exposing (isPartOfSnake, cellsAreEqual)

import Types exposing (Cell, Snake)


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
