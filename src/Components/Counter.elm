module Counter exposing (..)

import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Html exposing (map)


type alias Model =
    { count : Int
    }


initWithCount : Int -> Model
initWithCount initialCount =
    { count = initialCount }


getCount : Model -> Int
getCount model =
    model.count


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


view : Model -> (Msg -> msg) -> Html msg
view model upgrade =
    map upgrade <|
        div []
            [ p [] [ text ("Count: " ++ toString model.count) ]
            , button [ onClick Increment ]
                [ text "+" ]
            , button [ onClick Decrement ]
                [ text "-" ]
            ]
