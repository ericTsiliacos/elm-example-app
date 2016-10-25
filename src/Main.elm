module Main exposing (..)

import Counter exposing (init, update, view)
import Html.App exposing (beginnerProgram)


main : Program Never
main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
