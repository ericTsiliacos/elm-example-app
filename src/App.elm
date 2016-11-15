port module App exposing (..)

import Html exposing (Attribute, Html, button, div, input, label, li, p, span, text, ul)
import Html.Attributes exposing (checked, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, bool, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode exposing (Value, encode, object)
import String exposing (reverse)
import Platform exposing (Task)
import Task


type alias Model =
    { count : Int
    , text : String
    , showText : Bool
    , people : People
    }


type alias AppState =
    { count : Int
    , text : String
    , showText : Bool
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


appStateDecoder : Decoder AppState
appStateDecoder =
    decode AppState
        |> required "count" int
        |> required "text" string
        |> required "showText" bool


initialModel : Model
initialModel =
    { count = 0
    , text = ""
    , showText = True
    , people = []
    }


init : Maybe AppState -> ( Model, Cmd Msg )
init appState =
    case appState of
        Just appState ->
            { initialModel
                | count = appState.count
                , text = appState.text
                , showText = appState.showText
            }
                ! [ getPeople ]

        Nothing ->
            initialModel ! [ getPeople ]


getPeople : Cmd Msg
getPeople =
    Http.send GetPeople <| get "http://localhost:8888/people" peopleDecoder


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
    | GetPeople (Result Http.Error People)
    | LoadLocalStorageAppState String
    | Reset


port saveStateToLocalStorage : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            storeAndReturn
                { model
                    | text = initialModel.text
                    , count = initialModel.count
                    , showText = initialModel.showText
                }

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

        GetPeople (Ok people) ->
            { model | people = people } ! []

        GetPeople (Err _) ->
            model ! []

        LoadLocalStorageAppState appStateJSONString ->
            let
                appStateResult =
                    Json.Decode.decodeString appStateDecoder appStateJSONString
            in
                case appStateResult of
                    Ok appState ->
                        { model
                            | text = appState.text
                            , count = appState.count
                            , showText = appState.showText
                        }
                            ! []

                    Err _ ->
                        model ! []


storeAndReturn : Model -> ( Model, Cmd Msg )
storeAndReturn model =
    model
        ! [ saveStateToLocalStorage <| encodeModel model
          ]


view : Model -> Html Msg
view model =
    div [ containerStyle ]
        [ button [ onClick Reset ] [ text "Reset" ]
        , div []
            [ p [] [ text ("Count: " ++ toString model.count) ]
            , button [ onClick Increment ]
                [ text "+" ]
            , button [ onClick Decrement ]
                [ text "-" ]
            ]
        , p []
            [ label [] [ text "Toggle Showing Text Output" ]
            , input
                [ type_ "checkbox"
                , checked model.showText
                , onCheck ToggleCheckBox
                ]
                []
            ]
        , div
            [ style
                [ visibility model.showText
                ]
            ]
            [ p [] [ text <| "Text: " ++ model.text ]
            , input
                [ onInput TextChange
                , value model.text
                ]
                []
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
