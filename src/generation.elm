module Generation exposing (Column, Gen, Height, Row, Width, getDimensions, init)

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


init : Height -> Width -> Gen
init height width =
    Array2D.repeat height width Dead |> Gen


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



{--




mapRows : (Row -> a) -> Gen -> List a
mapRows f (Gen { height }) =
    List.range 0 (height - 1)
        |> List.map f


mapRowCells : Row -> (CellState -> a) -> Gen -> List a
mapRowCells row f (Gen { width, grid }) =
    grid
        |> Array.slice (row * width) width
        |> Array.toList
        |> List.map f


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


possibleCellNeighbors : Pos -> List Pos
possibleCellNeighbors pos =
    let
        deltaPos =
            cartesian [ -1, 0, 1 ] [ -1, 0, 1 ]
    in
    deltaPos
        |> List.filter ((/=) ( 0, 0 ))
        |> List.map (\( deltaX, deltaY ) -> { row = pos.row + deltaY, col = pos.col + deltaX })


isValidPos : Gen -> Pos -> Bool
isValidPos (Gen { width, height }) pos =
    pos.row >= 0 && pos.row < height && pos.col >= 0 && pos.col < width


filterPos : Gen -> List Pos -> List Pos
filterPos gen =
    List.filter (isValidPos gen)


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


validCellNeighbors : Pos -> Gen -> List Pos
validCellNeighbors =
    possibleCellNeighbors >> flip filterPos


type alias AliveNeighbors =
    Int


aliveNeighbors : Gen -> Pos -> AliveNeighbors
aliveNeighbors gen pos =
    validCellNeighbors pos gen
        |> List.map (gridGet gen)
        |> List.filter ((==) Alive)
        |> List.length


cellAliveNeighborsInfo : Gen -> Pos -> ( CellState, AliveNeighbors )
cellAliveNeighborsInfo gen pos =
    let
        cellState =
            gridGet gen pos
    in
    ( cellState, aliveNeighbors gen pos )


stateFromNeighborsInfo : ( CellState, AliveNeighbors ) -> CellState
stateFromNeighborsInfo neighborsInfo =
    case neighborsInfo of
        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        ( Dead, 3 ) ->
            Alive

        _ ->
            Dead


calculateCellNewState : Gen -> Pos -> CellState
calculateCellNewState gen =
    cellAliveNeighborsInfo gen >> stateFromNeighborsInfo

--}
