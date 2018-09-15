module Generation exposing (CellState(..), Column, Gen, Height, Row, Width, flatten, getDimensions, nextGen, repeat)

import Array exposing (Array)
import Array2D exposing (Array2D)


type CellState
    = Dead
    | Alive


type alias Height =
    Int


type alias Width =
    Int


type alias Row =
    Int


type alias Column =
    Int


type Gen
    = Gen (Array2D CellState)


repeat : Height -> Width -> CellState -> Gen
repeat height width state =
    Array2D.repeat height width state |> Gen


getDimensions : Gen -> ( Height, Width )
getDimensions (Gen array) =
    ( Array2D.rows array, Array2D.columns array )


getCellState : Row -> Column -> Gen -> CellState
getCellState row col (Gen array) =
    array |> Array2D.get row col |> Maybe.withDefault Dead


setCellState : Row -> Column -> CellState -> Gen -> Gen
setCellState row col state (Gen array) =
    array |> Array2D.set row col state |> Gen


toggleCellState : Row -> Column -> Gen -> Gen
toggleCellState row col gen =
    let
        newState =
            gen |> getCellState row col |> flipCellState
    in
    gen |> setCellState row col newState


flipCellState : CellState -> CellState
flipCellState state =
    if state == Dead then
        Alive

    else
        Dead


countAliveCells : List CellState -> Int
countAliveCells =
    let
        accumulateAlive state acc =
            if state == Alive then
                acc + 1

            else
                acc
    in
    List.foldl accumulateAlive 0


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


cellNeighbors : Row -> Column -> List ( Row, Column )
cellNeighbors row col =
    cartesian [ -1, 0, 1 ] [ -1, 0, 1 ]
        |> List.filter ((/=) ( 0, 0 ))
        |> List.map (\( deltaRow, deltaCol ) -> ( row + deltaRow, col + deltaCol ))


cellNeighborsStates : Row -> Column -> Gen -> List CellState
cellNeighborsStates row col gen =
    cellNeighbors row col |> List.map (\( r, c ) -> getCellState r c gen)


getCellNewState : Gen -> Row -> Column -> CellState -> CellState
getCellNewState gen row col currentState =
    cellNeighborsStates row col gen
        |> countAliveCells
        |> calculateNewState currentState


type alias AliveNeighbors =
    Int


calculateNewState : CellState -> AliveNeighbors -> CellState
calculateNewState currentstate aliveNeighbors =
    case ( currentstate, aliveNeighbors ) of
        ( Dead, 3 ) ->
            Alive

        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        _ ->
            Dead


nextGen : Gen -> Gen
nextGen ((Gen array) as gen) =
    array
        |> Array2D.indexedMap (getCellNewState gen)
        |> Gen


flatten : (Row -> Column -> CellState -> a) -> Gen -> List a
flatten f (Gen array) =
    let
        rows =
            array |> Array2D.rows

        mapRow row =
            array
                |> Array2D.getRow row
                |> Maybe.withDefault Array.empty
                |> Array.indexedMap (\col state -> f row col state)
                |> Array.toList
    in
    List.range 0 (rows - 1)
        |> List.concatMap mapRow
