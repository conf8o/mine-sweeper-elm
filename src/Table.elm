module Table exposing (..)
import Array exposing (..)
import Set exposing (Set, member)

type CellStatus = Opened | Closed | Flagged

type Content = Hint Int | Bomb | Empty

type alias Cell = (CellStatus, Content)

type alias Height = Int
type alias Width = Int
type alias TableSize = (Height, Width)
type alias Table = { size : TableSize, table: Array (Array Cell) }

type alias Point = (Int, Int)

open : Cell -> Cell
open cell =
    case cell of
        (Closed, content) -> (Opened, content)
        _ -> cell

flag : Cell -> Cell
flag cell =
    case cell of
        (Closed, content) -> (Flagged, content)
        _ -> cell

isInside : TableSize -> Point -> Bool
isInside (height, width) (y, x) =
     x >= 0 && y >= 0 && x < width && y < height

aroundDirections : List (Int, Int)
aroundDirections =
    List.concatMap (\y -> List.map (\x -> (y, x)) [-1, 0 , 1]) [-1, 0 , 1]

aroundPoints : TableSize -> Point -> List Point
aroundPoints size (y, x) =
    aroundDirections
    |> List.map (\(dy, dx) -> (dy + y, dx + x))
    |> List.filter (isInside size)

isBomb : Cell -> Bool
isBomb cell =
    case cell of
        (_, Bomb) -> True
        _ -> False

countBombs : List Cell -> Int
countBombs cells =
    cells
    |> List.map (\cell -> if (isBomb cell) then 1 else 0)
    |> List.sum

countAroundBombs : TableSize -> Set Point -> Point -> Int
countAroundBombs size bombPoints point =
    aroundPoints size point
    |> List.filter (\p -> (member p bombPoints))
    |> List.length

initTable : TableSize -> Set Point -> Table
initTable (height, width) bombPoints =
    List.range 0 (height - 1)
    |> List.map (\y ->
        List.range 0 (width - 1)
        |> List.map (\x ->
            if (member (y, x) bombPoints) then
                (Closed, Bomb)
            else
                let
                    bombCount = countAroundBombs (height, width) bombPoints (y, x)
                in
                    if bombCount > 0 then
                        (Closed, Hint bombCount)
                    else
                        (Closed, Empty)
        )
        |> Array.fromList
    )
    |> Array.fromList
    |> Table (height, width)

