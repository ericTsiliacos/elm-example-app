module Counter exposing (..)

import Html exposing (Html, button, div, p, text, map)
import Html.Events exposing (onClick)


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            case model of
                0 ->
                    model

                _ ->
                    model - 1


view : Model -> (Msg -> msg) -> Html msg
view model upgrade =
    map upgrade <|
        div []
            [ p [] [ text ("Count: " ++ toString model) ]
            , button [ onClick Increment ]
                [ text "+" ]
            , button [ onClick Decrement ]
                [ text "-" ]
            ]
