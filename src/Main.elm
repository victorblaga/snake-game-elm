module Main exposing (main)

import Browser
import Model exposing (createRandomFood, init)
import Types exposing (Model, Msg(..))
import Utils exposing (isPartOfSnake)
import View exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ halfSize, snake } as model) =
    case msg of
        NewFood newFood ->
            if isPartOfSnake newFood snake then
                ( model, createRandomFood halfSize )

            else
                ( { model | food = newFood }, Cmd.none )

        _ ->
            ( model, Cmd.none )
