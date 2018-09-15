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
            Initial (Generation.repeat 8 8 Generation.Dead)
    in
    Browser.sandbox { init = initState, update = update, view = view >> toUnstyled }


update _ model =
    model


view model =
    case model of
        Initial gen ->
            div []
                [ h1 [ css [ property "text-align" "center" ] ] [ text "Game of LIFE" ]
                , viewGrid gen
                ]


viewGrid : Gen -> Html msg
viewGrid gen =
    Html.Styled.table
        [ css
            [ width (pct 80)
            , margin auto
            ]
        ]
        (gen
            |> Generation.mapRows (createRow gen)
        )


createRow : Gen -> Row -> Html msg
createRow gen row =
    tr [] (createCells gen row)


createCells : Gen -> Row -> List (Html msg)
createCells gen row =
    Generation.mapRowCells (createCell gen) row gen


createCell : Gen -> Row -> Column -> CellState -> Html msg
createCell gen _ _ state =
    let
        ( _, gWidth ) =
            Generation.getDimensions gen

        widthRatio =
            pct (100.0 / toFloat gWidth)
    in
    td
        [ css
            [ width widthRatio
            , paddingBottom widthRatio
            , position relative
            , colorFromState state |> backgroundColor
            ]
        ]
        []


colorFromState : CellState -> Color
colorFromState state =
    case state of
        Dead ->
            rgb 100 80 80

        Alive ->
            rgb 200 150 0


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
            rgb 0 255 255
