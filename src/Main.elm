module Main exposing (..)

import Counter exposing (Model, Msg, init, update, view)
import Navigation exposing (Location)
import String
import UrlParser exposing (Parser, format, oneOf, parse, s)


type alias Model =
    { route : Route
    , counterModel : Counter.Model
    }


type Msg
    = CounterMsg Counter.Msg
    | Navigate String


type Route
    = Home
    | MultiCounter


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ format Home (s "")
        , format MultiCounter (s "multi_counter")
        ]


encode : Route -> String
encode route =
    case route of
        Home ->
            "/"

        MultiCounter ->
            "/multi_counter"


decode : Location -> Result String Int
decode location =
    parse identity


urlParser : Navigation.Parser (Result String Int)
urlParser =
    Navigation.makeParser decode


initialModel : Result String Route -> ( Model, Cmd Msg )
initialModel =
    { route = Home
    , counterModel = Counter.init
    }


init : Result String Route -> ( Model, Cmd Msg )
init result =
    urlUpdate result initialModel


urlUpdate : Result String Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err _ ->
            ( model, Cmd.none )

        Ok (MultiCounter as route) ->
            { model | route = route } ! []

        Ok route ->
            { model | route = route } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg m ->
            let
                ( subMdl, subCmd ) =
                    Counter.update m model.counterModel
            in
                { model | counterModel = subMdl }
                    ! [ Cmd.map CounterMsg subCmd ]

        Navigate url ->
            model ! [ Navigation.newUrl url ]


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , subscriptions = Sub.none always
        , update = update
        , urlUpdate = \x y -> init
        , view = view
        }
