module Main exposing (..)

import App
    exposing
        ( init
        , update
        , view
        , subscriptions
        , AppState
        , Flags
        , Msg(UrlChange)
        )
import Navigation exposing (programWithFlags)


main : Program Flags App.Model App.Msg
main =
    programWithFlags UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
