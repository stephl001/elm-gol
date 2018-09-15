module Main exposing (main)

import Browser
import Css exposing (..)
import Generation exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)


type GameState
    = Initial Gen


main =
    let
        initState =
            Initial (Generation.repeat 1 5 Generation.Dead)
    in
    Browser.sandbox { init = initState, update = update, view = view >> toUnstyled }


update _ model =
    model


view model =
    let
        mystyle =
            css
                [ display block
                , margin auto
                , width (pct 40)
                ]
    in
    case model of
        Initial gen ->
            div []
                [ h1 [ css [ property "text-align" "center" ] ] [ text "Game of LIFE" ]
                , div [ mystyle ] (Generation.flatten viewCell gen)
                , div [ mystyle ] (Generation.flatten viewCell gen)
                ]


viewCell : Row -> Column -> CellState -> Html msg
viewCell _ _ state =
    let
        mystyle =
            css
                [ display inlineBlock

                --, padding (px 5)
                , border3 (px 2) solid (rgb 120 120 120)
                , borderRadius (px 10)
                , width (px 100)
                , height (px 100)
                , backgroundColor (cellColorFromState state)
                ]
    in
    div [ mystyle ] []


cellColorFromState : CellState -> Color
cellColorFromState state =
    case state of
        Alive ->
            rgb 0 0 0

        Dead ->
            rgb 255 255 255


stringifyState : CellState -> String
stringifyState s =
    case s of
        Alive ->
            "Alive"

        Dead ->
            "Dead"
