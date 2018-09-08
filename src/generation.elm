module Generation exposing (Gen,init)

import Array exposing (Array)

type CellState
    = Dead
    | Alive

type alias Height = Int
type alias Width = Int
type alias Row = Int
type alias Column = Int
type alias Pos = 
    { row: Row
    , col: Column 
    }

type Gen =
    Gen 
        { grid: Array CellState
        , width: Width
        , height: Height
        }

init: Width -> Height -> Gen
init width height = 
    { grid = Array.repeat (width*height) Dead
    , width = width
    , height = height
    } |> Gen

indexFromRowCol: Gen -> Pos -> Int
indexFromRowCol (Gen {width}) {row,col} =
    row*width + col

posFromIndex: Gen -> Int -> Pos
posFromIndex (Gen {width}) index =
    {row=index//width, col=index % width}

gridGet: Pos -> Gen -> CellState
gridGet pos (Gen {grid} as gen) =
    let
        index = indexFromRowCol gen pos
    in
        grid |> Array.get index |> Maybe.withDefault Dead

gridSet: Pos -> Gen -> CellState -> Gen
gridSet pos (Gen ({grid} as innerGen) as gen) state =
    let
        index = indexFromRowCol gen pos
        newGrid = grid |> Array.set index state
    in
        {innerGen | grid=newGrid} |> Gen
    
toggleCellState: Pos -> Gen -> Gen
toggleCellState pos gen =
    gridGet pos gen
    |> flipCellState
    |> gridSet pos gen

flipCellState: CellState -> CellState
flipCellState state =
    if state == Dead then Alive else Dead
    
mapRows: (Row -> a) -> Gen -> List a
mapRows f (Gen {height}) =
    List.range 0 (height-1)
    |> List.map f

mapRowCells: Row -> (CellState -> a) -> Gen -> List a
mapRowCells row f (Gen {width,grid}) =
    grid
    |> Array.slice (row*width) width
    |> Array.toList
    |> List.map f

map: (Pos -> CellState -> CellState) -> Gen -> Gen
map fn (Gen ({grid} as innerGen) as gen) =
    let
        indexToPos = posFromIndex gen
        newGrid = grid |> Array.indexedMap (indexToPos>>fn)
    in    
        {innerGen | grid = newGrid} |> Gen

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

possibleCellNeighbors: Pos -> List Pos
possibleCellNeighbors pos =
    let 
        deltaPos = cartesian [-1,0,1] [-1,0,1]
    in
        deltaPos 
        |> List.map (\(deltaX,deltaY) -> {row=pos.row+deltaY,col=pos.col+deltaX})

filterPos: List Pos -> Gen -> List Pos
filterPos positions (Gen {width,height}) =
    let
        isValidPos: Pos -> Bool
        isValidPos pos =
            pos.row >= 0 && pos.row < height && pos.col >=0 && pos.col < width
    in
        positions |> List.filter isValidPos