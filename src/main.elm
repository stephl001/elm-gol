module Main exposing (main)

import Browser
import Css exposing (..)
import Generation exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, disabled, href, src)
import Html.Styled.Events exposing (onClick)


type Msg
    = ToggleCell Row Column
    | NextGeneration


main =
    let
        initState =
            Generation.repeat 8 8 Generation.Dead
    in
    Browser.sandbox { init = initState, update = update, view = view >> toUnstyled }


update : Msg -> Gen -> Gen
update msg gen =
    case msg of
        ToggleCell row col ->
            Generation.toggleCellState row col gen

        NextGeneration ->
            Generation.nextGen gen


view gen =
    div [ css [ property "text-align" "center", height (pct 100), margin (px 0) ] ]
        [ h1 [] [ text "Game of LIFE" ]
        , button [ onClick NextGeneration, disabled True ] [ text "Next Generation" ]
        , viewGrid gen
        ]


hasLivingCells : Gen -> Bool
hasLivingCells =
    Generation.foldl ((==) Alive >> (||)) False


viewGrid : Gen -> Html Msg
viewGrid gen =
    Html.Styled.table
        [ css
            [ width (px 500)
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
