module Counter exposing (..)

import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


type Msg
    = Increment
    | Decrement


init : data -> ( Model, Cmd Msg )
init data =
    ( { count = 0 }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Decrement ->
            case model.count of
                0 ->
                    ( model
                    , Cmd.none
                    )

                _ ->
                    ( { model | count = model.count - 1 }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Counter" ]
        , button [] [ text "Multi-Counter" ]
        , div []
            [ text <| toString model.count ]
        , button [ onClick Increment ]
            [ text "Increment" ]
        , button [ onClick Decrement ]
            [ text "Decrement" ]
        ]
