module Counter exposing (..)

import Html exposing (Html, button, div, input, label, p, span, text)
import Html.Attributes exposing (checked, style, type')
import Html.Events exposing (onCheck, onClick, onInput)
import String exposing (reverse)


type alias Model =
    { count : Int
    , text : String
    , showText : Bool
    }


init : Model
init =
    { count = 0
    , text = ""
    , showText = True
    }


type Msg
    = Increment
    | Decrement
    | TextChange String
    | ToggleCheckBox Bool


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

        TextChange value ->
            { model | text = reverse value }

        ToggleCheckBox checked ->
            { model | showText = checked }


view : Model -> Html Msg
view model =
    div [ style [ ( "background-color", "rgb(88, 211, 216)" ) ] ]
        [ div []
            [ p [] [ text ("Count: " ++ toString model.count) ]
            , button [ onClick Increment ]
                [ text "+" ]
            , button [ onClick Decrement ]
                [ text "-" ]
            ]
        , p []
            [ label [] [ text "Toggle Showing Text Output" ]
            , input [ type' "checkbox", checked True, onCheck ToggleCheckBox ] []
            ]
        , div
            [ style
                [ visibility model.showText
                ]
            ]
            [ p [] [ text <| "Text: " ++ model.text ]
            , input [ onInput TextChange ] []
            ]
        ]


visibility : Bool -> ( String, String )
visibility predicate =
    if predicate then
        ( "display", "block" )
    else
        ( "display", "none" )
