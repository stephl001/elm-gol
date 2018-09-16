module Main exposing (main)

import Browser
import Css exposing (..)
import Generation exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)


type GameState
    = Edition Gen


type Msg
    = ToggleCell Row Column


main =
    let
        initState =
            Edition (Generation.repeat 8 8 Generation.Dead)
    in
    Browser.sandbox { init = initState, update = update, view = view >> toUnstyled }


update : Msg -> GameState -> GameState
update msg model =
    case msg of
        ToggleCell row col ->
            case model of
                Edition gen ->
                    Edition (Generation.toggleCellState row col gen)


view model =
    case model of
        Edition gen ->
            div []
                [ h1 [ css [ property "text-align" "center" ] ] [ text "Game of LIFE" ]
                , viewGrid gen
                ]


viewGrid : Gen -> Html Msg
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


createRow : Gen -> Row -> Html Msg
createRow gen row =
    tr [] (createCells gen row)


createCells : Gen -> Row -> List (Html Msg)
createCells gen row =
    Generation.mapRowCells (createCell gen) row gen


createCell : Gen -> Row -> Column -> CellState -> Html Msg
createCell gen row col state =
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
        , onClick (ToggleCell row col)
        ]
        []


colorFromState : CellState -> Color
colorFromState state =
    case state of
        Dead ->
            rgb 100 80 80

        Alive ->
            rgb 200 150 0
