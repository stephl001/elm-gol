module Generation exposing (init,toggleCellState,mapRows,mapRowCells)

import Array exposing (Array)

type CellState
    = Dead
    | Alive

type alias Height = Int
type alias Width = Int
type alias Row = Int
type alias Column = Int

type alias CellInfo =
    { row: Row
    , col: Column
    , state: CellState
    }

type alias Gen =
    { grid: Array CellState
    , width: Width
    , height: Height
    }

init: Width -> Height -> Gen
init width height = 
    { grid=Array.repeat (width*height) Dead
    , width=width
    , height=height
    }

indexFromRowCol: Gen -> Row -> Column -> Int
indexFromRowCol {width} row col =
    row*width + col

gridGet: Gen -> Row -> Column -> CellInfo
gridGet ({grid} as gen) row col =
    let
        index = indexFromRowCol gen row col
    in
        grid |> Array.get index |> Maybe.withDefault Dead |> CellInfo row col

gridSet: Gen -> CellInfo -> Gen
gridSet ({grid} as gen) {row,col,state} =
    let
        index = indexFromRowCol gen row col
        newGrid = grid |> Array.set index state
    in
        {gen | grid=newGrid}
    
toggleCellState: Gen -> Row -> Column -> Gen
toggleCellState gen row col =
    gridGet gen row col
    |> flipCellState
    |> gridSet gen 

flipCellState: CellInfo -> CellInfo
flipCellState ({row,col,state} as cellInfo) =
    let
        newState = if state == Dead then Alive else Dead
    in
        {cellInfo | state=newState}

mapRows: (Row -> a) -> Gen -> List a
mapRows f {height} =
    List.range 0 (height-1)
    |> List.map f

mapRowCells: Row -> (CellState -> a) -> Gen -> List a
mapRowCells row f {width,grid} =
    grid
    |> Array.slice (row*width) width
    |> Array.toList
    |> List.map f