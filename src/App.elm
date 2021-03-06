port module App exposing (..)

import Html exposing (Attribute, Html, button, div, input, label, li, p, span, text, ul, map)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf, parseHash, top, parsePath)
import Html.Attributes exposing (checked, style, type_, value, min, max)
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (Error, get)
import Json.Decode exposing (Decoder, at, bool, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode exposing (Value, encode, object)
import String exposing (reverse)
import Navigation
import Counter exposing (..)
import RemoteData exposing (..)


-- Model


type alias Model =
    { counts : List Int
    , numberOfCounters : Int
    , reverseText : String
    , text : String
    , showText : Bool
    , people : WebData People
    , currentRoute : Route
    }


initialModel : Model
initialModel =
    { counts = []
    , numberOfCounters = 0
    , reverseText = ""
    , text = ""
    , showText = True
    , people = Loading
    , currentRoute = Home
    }


type alias AppState =
    { counts : List Int
    , numberOfCounters : Int
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
        [ ( "counts"
          , Json.Encode.list <|
                (List.map (\count -> Json.Encode.int count) model.counts)
          )
        , ( "numberOfCounters", Json.Encode.int model.numberOfCounters )
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
        |> required "counts" (Json.Decode.list Json.Decode.int)
        |> required "numberOfCounters" Json.Decode.int
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
    peopleDecoder
        |> get apiEndpoint
        |> RemoteData.sendRequest
        |> Cmd.map PeopleResponse



-- Routing


type Route
    = Home
    | First
    | Second


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map Home (s "index.html")
        , UrlParser.map First (s "first")
        , UrlParser.map Second (s "second")
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
                | counts = appState.counts
                , numberOfCounters = appState.numberOfCounters
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
    = CountersMsg Int Counter.Msg
    | CountersSliderChange String
    | TextChange String
    | ToggleCheckBox Bool
    | PeopleResponse (WebData People)
    | LoadLocalStorageAppState String
    | UrlChange Navigation.Location
    | NewUrl String
    | Reset



-- Ports


port saveStateToLocalStorage : String -> Cmd msg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            { model
                | reverseText = initialModel.reverseText
                , counts = []
                , numberOfCounters = 0
                , showText = initialModel.showText
                , text = initialModel.text
            }
                |> storeInLocalStorage

        CountersSliderChange value ->
            let
                newNumberOfCounters =
                    String.toInt value |> Result.withDefault 0

                newCounts =
                    if List.length model.counts < newNumberOfCounters then
                        model.counts ++ (List.repeat (newNumberOfCounters - List.length model.counts) 0)
                    else
                        List.take newNumberOfCounters model.counts
            in
                { model
                    | numberOfCounters = newNumberOfCounters
                    , counts = newCounts
                }
                    |> storeInLocalStorage

        CountersMsg counterId subMsg ->
            let
                countsWithIndex =
                    List.indexedMap (,) model.counts

                counts =
                    List.filter (\( index, _ ) -> index == counterId) countsWithIndex

                count =
                    case counts of
                        [ ( _, count ) ] ->
                            count

                        _ ->
                            0

                newCounterState =
                    Counter.update subMsg count

                newCounts =
                    List.map
                        (\( index, count ) ->
                            if index == counterId then
                                newCounterState
                            else
                                count
                        )
                        countsWithIndex
            in
                { model | counts = newCounts }
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

        PeopleResponse response ->
            { model | people = response } ! []

        LoadLocalStorageAppState appStateJSONString ->
            let
                appStateResult =
                    Json.Decode.decodeString appStateDecoder appStateJSONString
            in
                case appStateResult of
                    Ok appState ->
                        { model
                            | reverseText = appState.reverseText
                            , counts = appState.counts
                            , numberOfCounters = appState.numberOfCounters
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
    let
        counters =
            List.map (\( index, count ) -> Html.map (CountersMsg index) (Counter.view count)) <|
                List.indexedMap (,) model.counts
    in
        div []
            [ p [] [ text "Counters Controller" ]
            , input
                [ type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "5"
                , value <| toString model.numberOfCounters
                , onInput CountersSliderChange
                ]
                []
            , div [] counters
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
    case model.people of
        NotAsked ->
            text "Initialising."

        Loading ->
            text "Loading..."

        Failure err ->
            text ("Error: " ++ toString err)

        Success people ->
            div []
                [ p [] [ text <| "GET /people: " ]
                , ul [] (List.map (\person -> li [] [ text person.name ]) people)
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
