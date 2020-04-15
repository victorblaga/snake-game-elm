module View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class, classList)
import Types exposing (Cell, Model, Msg(..))
import Utils exposing (cellsAreEqual, isPartOfSnake)


view : Model -> Html Msg
view ({ gameOver } as model) =
    div
        [ classList
            [ ( "grid", True )
            , ( "game-over", gameOver )
            ]
        ]
        (createRows model)


createRows : Model -> List (Html Msg)
createRows ({ halfSize } as model) =
    List.map
        (\rowIndex -> createRow rowIndex model)
        (List.range -halfSize halfSize)


createRow : Int -> Model -> Html Msg
createRow rowIndex ({ halfSize } as model) =
    div
        [ class "row"
        , attribute "data-row" (String.fromInt rowIndex)
        ]
        (createCells rowIndex model)


createCells : Int -> Model -> List (Html Msg)
createCells rowIndex ({ halfSize } as model) =
    List.map
        (\colIndex -> createCell ( rowIndex, colIndex ) model)
        -- <-- Pass the model
        (List.range -halfSize halfSize)


createCell : Cell -> Model -> Html Msg
createCell cell { snake, food } =
    let
        ( rowIndex, colIndex ) =
            cell

        cellClasses =
            [ ( "cell", True )
            , ( "snake", isPartOfSnake cell snake )
            , ( "food", cellsAreEqual cell food )
            ]
    in
    div
        [ classList cellClasses
        , attribute "data-row" (String.fromInt rowIndex)
        , attribute "data-col" (String.fromInt colIndex)
        ]
        [ cellContents rowIndex colIndex ]


cellContents : Int -> Int -> Html Msg
cellContents rowIndex colIndex =
    text (String.fromInt rowIndex ++ "," ++ String.fromInt colIndex)
