import Html exposing (text)
import Html.App exposing (beginnerProgram)


main : Program Never
main = beginnerProgram
    { model = {}
    , update = \msg model -> {}
    , view = \_ -> text "Hello World!"
    }
