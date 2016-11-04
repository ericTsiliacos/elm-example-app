port module App exposing (..)

import Html exposing (Attribute, Html, button, div, input, label, li, p, span, text, ul)
import Html.Attributes exposing (checked, style, type')
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, int, list, object2, string, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode exposing (Value, bool, encode, object)
import String exposing (reverse)
import Platform exposing (Task)
import Task


type alias Model =
    { count : Int
    , text : String
    , showText : Bool
    , people : People
    }


type alias People =
    List Person


type alias Person =
    { id : Int
    , name : String
    }


modelToValue : Model -> Value
modelToValue model =
    object
        [ ( "count", Json.Encode.int model.count )
        , ( "text", Json.Encode.string model.text )
        , ( "showText", Json.Encode.bool model.showText )
        ]


encodeModel : Model -> String
encodeModel model =
    encode 0 (modelToValue model)


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


port saveStateToLocalStorage : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            storeAndReturn { model | count = model.count + 1 }

        Decrement ->
            case model.count of
                0 ->
                    storeAndReturn model

                _ ->
                    storeAndReturn { model | count = model.count - 1 }

        TextChange value ->
            storeAndReturn { model | text = reverse value }

        ToggleCheckBox checked ->
            storeAndReturn { model | showText = checked }

        GetPeopleSuccess people ->
            { model | people = people } ! []

        GetPeopleFailure _ ->
            model ! []


storeAndReturn : Model -> ( Model, Cmd Msg )
storeAndReturn model =
    model
        ! [ saveStateToLocalStorage <| encodeModel model
          ]


view : Model -> Html Msg
view model =
    div [ containerStyle ]
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


containerStyle : Attribute Msg
containerStyle =
    style [ ( "background-color", "rgb(88, 211, 216)" ), ( "padding", "1rem" ) ]
