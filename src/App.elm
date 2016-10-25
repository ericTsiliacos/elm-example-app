module App exposing (..)

import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


init : Model
init =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            case model.count of
                0 ->
                    model

                _ ->
                    { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Counter" ]
        , div []
            [ text <| toString model.count ]
        , button [ onClick Increment ]
            [ text "Increment" ]
        , button [ onClick Decrement ]
            [ text "Decrement" ]
        ]
