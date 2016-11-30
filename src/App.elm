port module App exposing (..)

import Html exposing (Attribute, Html, button, div, input, label, li, p, span, text, ul)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf, parseHash, top, parsePath)
import Html.Attributes exposing (checked, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, bool, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode exposing (Value, encode, object)
import String exposing (reverse)
import Navigation
import Counter exposing (..)


-- Model


type alias Model =
    { count : Int
    , count1 : Int
    , reverseText : String
    , text : String
    , showText : Bool
    , people : People
    , getPeopleErrorMsg : String
    , currentRoute : Route
    }


initialModel : Model
initialModel =
    { count = 0
    , count1 = 0
    , reverseText = ""
    , text = ""
    , showText = True
    , people = []
    , getPeopleErrorMsg = ""
    , currentRoute = Home
    }


type alias AppState =
    { count : Int
    , count1 : Int
    , reverseText : String
    , showText : Bool
    }


type alias People =
    List Person


type alias Person =
    { id : Int
    , name : String
    }



-- Encoders


modelToObject : Model -> Value
modelToObject model =
    object
        [ ( "count", Json.Encode.int model.count )
        , ( "count1", Json.Encode.int model.count1 )
        , ( "reverseText", Json.Encode.string model.reverseText )
        , ( "showText", Json.Encode.bool model.showText )
        ]


encodeModel : Model -> String
encodeModel model =
    encode 0 (modelToObject model)



-- Decoders


appStateDecoder : Decoder AppState
appStateDecoder =
    decode AppState
        |> required "count" Json.Decode.int
        |> required "count1" Json.Decode.int
        |> required "reverseText" Json.Decode.string
        |> required "showText" Json.Decode.bool


peopleDecoder : Decoder People
peopleDecoder =
    at [ "data" ] (list personDecoder)


personDecoder : Decoder Person
personDecoder =
    decode Person
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string



-- Http Requests


getPeople : String -> Cmd Msg
getPeople apiEndpoint =
    Http.send GetPeople <| get apiEndpoint peopleDecoder



-- Routing


type Route
    = Home
    | First
    | Second


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home (s "index.html")
        , map First (s "first")
        , map Second (s "second")
        ]



-- Init


type alias Flags =
    { appState : Maybe AppState
    , apiEndpoint : String
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    case flags.appState of
        Just appState ->
            { initialModel
                | count = appState.count
                , count1 = appState.count1
                , reverseText = appState.reverseText
                , showText = appState.showText
                , currentRoute =
                    Maybe.withDefault Home <|
                        parsePath route location
            }
                ! [ getPeople flags.apiEndpoint ]

        Nothing ->
            initialModel ! [ getPeople flags.apiEndpoint ]



-- Messages


type Msg
    = CounterMsg Counter.Msg
    | CounterMsg1 Counter.Msg
    | TextChange String
    | ToggleCheckBox Bool
    | GetPeople (Result Http.Error People)
    | LoadLocalStorageAppState String
    | UrlChange Navigation.Location
    | NewUrl String
    | Reset



-- Ports


port saveStateToLocalStorage : String -> Cmd msg



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            { model
                | reverseText = initialModel.reverseText
                , count = 0
                , count1 = 0
                , showText = initialModel.showText
                , text = initialModel.text
            }
                |> storeInLocalStorage

        CounterMsg subMsg ->
            let
                newCounterState =
                    Counter.update subMsg model.count
            in
                { model | count = newCounterState }
                    |> storeInLocalStorage

        CounterMsg1 subMsg ->
            let
                newCounterState =
                    Counter.update subMsg model.count1
            in
                { model | count1 = newCounterState }
                    |> storeInLocalStorage

        TextChange value ->
            { model
                | reverseText = reverse value
                , text = value
            }
                |> storeInLocalStorage

        ToggleCheckBox checked ->
            { model | showText = checked }
                |> storeInLocalStorage

        GetPeople (Ok people) ->
            { model | people = people } ! []

        GetPeople (Err err) ->
            { model | getPeopleErrorMsg = toString err } ! []

        LoadLocalStorageAppState appStateJSONString ->
            let
                appStateResult =
                    Json.Decode.decodeString appStateDecoder appStateJSONString
            in
                case appStateResult of
                    Ok appState ->
                        { model
                            | reverseText = appState.reverseText
                            , count = appState.count
                            , count1 = appState.count1
                            , showText = appState.showText
                        }
                            ! []

                    Err _ ->
                        model ! []

        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            { model
                | currentRoute =
                    Maybe.withDefault Home <|
                        parsePath route location
            }
                ! []


storeInLocalStorage : Model -> ( Model, Cmd Msg )
storeInLocalStorage model =
    model
        ! [ saveStateToLocalStorage <| encodeModel model
          ]



-- View


view : Model -> Html Msg
view model =
    div [ containerStyle ]
        [ button [ onClick (NewUrl "/index.html") ] [ text "Home" ]
        , button [ onClick (NewUrl "/first") ] [ text "First" ]
        , button [ onClick (NewUrl "/second") ] [ text "Second" ]
        , mainContentRouter model
        ]


mainContentRouter : Model -> Html Msg
mainContentRouter model =
    case model.currentRoute of
        Home ->
            homeView model

        First ->
            firstView model

        Second ->
            secondView model


homeView : Model -> Html Msg
homeView model =
    div []
        [ button [ onClick Reset ] [ text "Reset" ]
        , countersView model
        , reverseTextInputView model
        , showPeopleView model
        ]


countersView : Model -> Html Msg
countersView model =
    div []
        [ Counter.view model.count CounterMsg
        , Counter.view model.count1 CounterMsg1
        ]


reverseTextInputView : Model -> Html Msg
reverseTextInputView model =
    div []
        [ p []
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
            [ p [] [ text <| "Reverse Text: " ++ model.reverseText ]
            , input
                [ onInput TextChange
                , value model.text
                ]
                []
            ]
        ]


showPeopleView : Model -> Html Msg
showPeopleView model =
    div []
        [ p [] [ text <| "GET /people: " ++ (model.getPeopleErrorMsg) ]
        , ul [] (List.map (\person -> li [] [ text person.name ]) model.people)
        ]


firstView : Model -> Html Msg
firstView _ =
    p [] [ text "First View" ]


secondView : Model -> Html Msg
secondView _ =
    p [] [ text "Second View" ]



-- View Helpers


visibility : Bool -> ( String, String )
visibility predicate =
    if predicate then
        ( "display", "block" )
    else
        ( "display", "none" )


containerStyle : Attribute Msg
containerStyle =
    style [ ( "background-color", "rgb(88, 211, 216)" ), ( "padding", "1rem" ) ]
