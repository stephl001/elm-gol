module Generation exposing (Gen,init,getDimensions,toggleCellState,gridGet,gridSet,mapRows,mapRowCells)

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

getDimensions: Gen -> (Width,Height)
getDimensions (Gen {width,height}) = (width,height)

indexFromRowCol: Gen -> Pos -> Int
indexFromRowCol (Gen {width}) {row,col} =
    row*width + col

posFromIndex: Gen -> Int -> Pos
posFromIndex (Gen {width}) index =
    {row=index//width, col=index % width}

gridGet: Gen -> Pos -> CellState
gridGet (Gen {grid} as gen) pos =
    let
        index = indexFromRowCol gen pos
    in
        grid |> Array.get index |> Maybe.withDefault Dead

gridSet: Gen -> Pos -> CellState -> Gen
gridSet (Gen ({grid} as innerGen) as gen) pos state =
    let
        index = indexFromRowCol gen pos
        newGrid = grid |> Array.set index state
    in
        {innerGen | grid=newGrid} |> Gen
    
toggleCellState: Gen -> Pos -> Gen
toggleCellState gen pos =
    gridGet gen pos
    |> flipCellState
    |> gridSet gen pos

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
        |> List.filter ((/=) (0,0))
        |> List.map (\(deltaX,deltaY) -> {row=pos.row+deltaY,col=pos.col+deltaX})

filterPos: List Pos -> Gen -> List Pos
filterPos positions (Gen {width,height}) =
    let
        isValidPos: Pos -> Bool
        isValidPos pos =
            pos.row >= 0 && pos.row < height && pos.col >=0 && pos.col < width
    in
        positions |> List.filter isValidPos

validCellNeighbors: Pos -> Gen -> List Pos
validCellNeighbors = possibleCellNeighbors >> filterPos

type alias AliveNeighbors = Int

aliveNeighbors: Gen -> Pos -> AliveNeighbors
aliveNeighbors gen pos =
    validCellNeighbors pos gen 
    |> List.map (gridGet gen) 
    |> List.filter ((==) Alive) 
    |> List.length

cellAliveNeighborsInfo: Gen -> Pos -> (CellState,AliveNeighbors)
cellAliveNeighborsInfo gen pos =
    let
        cellState = gridGet gen pos
    in
        (cellState,aliveNeighbors gen pos)

stateFromNeighborsInfo: (CellState,AliveNeighbors) -> CellState
stateFromNeighborsInfo neighborsInfo =
    case neighborsInfo of
        (Alive,2) -> Alive
        (Alive,3) -> Alive
        (Dead,3) -> Alive
        _ -> Dead