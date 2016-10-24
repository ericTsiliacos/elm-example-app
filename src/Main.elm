module Main exposing (..)

import Html exposing (Html, text)
import Html.App exposing (beginnerProgram)


type alias Model =
    {}


update : msg -> model -> model
update _ model =
    model


view : model -> Html msg
view _ =
    text "Hello World!"


main : Program Never
main =
    beginnerProgram
        { model = {}
        , update = update
        , view = view
        }
