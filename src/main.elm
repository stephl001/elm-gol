module Main exposing (initGen, main)

import Generation exposing (..)
import Html exposing (text)


initGen : Gen
initGen =
    Generation.init 10 10


main =
    text "Hello World"
