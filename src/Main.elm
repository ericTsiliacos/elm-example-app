module Main exposing (..)

import App exposing (init, subscriptions, update, view)
import Html.App exposing (program)


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
