module Counter exposing (..)

import Html exposing (Html, button, div, input, label, li, p, span, text, ul)
import Html.Attributes exposing (checked, style, type')
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, int, list, object2, string, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import String exposing (reverse)
import Platform exposing (Task)
import Task


type alias Model =
    { count : Int
    , text : String
    , showText : Bool
    , people : People
    }


initialModel : Model
initialModel =
    { count = 0
    , text = ""
    , showText = True
    , people = []
    }


init : ( Model, Cmd Msg )
init =
    initialModel ! [ getPeople ]


getPeople : Cmd Msg
getPeople =
    get peopleDecoder "http://localhost:8888/people"
        |> Task.perform GetPeopleFailure GetPeopleSuccess


type alias People =
    List Person


type alias Person =
    { id : Int
    , name : String
    }


peopleDecoder : Decoder People
peopleDecoder =
    at [ "data" ] (list personDecoder)


personDecoder : Decoder Person
personDecoder =
    decode Person
        |> required "id" int
        |> required "name" string


type Msg
    = Increment
    | Decrement
    | TextChange String
    | ToggleCheckBox Bool
    | GetPeopleFailure Error
    | GetPeopleSuccess People


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 } ! []

        Decrement ->
            case model.count of
                0 ->
                    model ! []

                _ ->
                    { model | count = model.count - 1 } ! []

        TextChange value ->
            { model | text = reverse value } ! []

        ToggleCheckBox checked ->
            { model | showText = checked } ! []

        GetPeopleSuccess people ->
            { model | people = people } ! []

        GetPeopleFailure _ ->
            model ! []


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
        , div []
            [ p [] [ text "Http Request: " ]
            , ul [] (List.map (\person -> li [] [ text person.name ]) model.people)
            ]
        ]


visibility : Bool -> ( String, String )
visibility predicate =
    if predicate then
        ( "display", "block" )
    else
        ( "display", "none" )
