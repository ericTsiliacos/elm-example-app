module Main exposing (..)

import Counter exposing (init, update, view)
import Html.App exposing (program)


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
