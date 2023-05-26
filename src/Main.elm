module Main exposing (..)
import Array exposing (..)
import Html exposing (text)
import Random
import Random.List


type alias Height = Int
type alias Width = Int
type alias TableSize = (Height, Width)
type alias Table = Array (Array Cell)

type alias BombCount = Int

easyTableSize : TableSize
easyTableSize = (10, 10)

easyBombCount : BombCount
easyBombCount = 10


type Content = Hint Int | Bomb | Empty

type Cell = Opened Content | Closed Content | Flagged Content

open : Cell -> Cell
open cell =
    case cell of
        Closed c -> Opened c
        x -> x

flag : Cell -> Cell
flag cell =
    case cell of
        Closed c -> Flagged c
        x -> x

addBombCount : Cell -> Int -> Int
addBombCount cell n =
    case cell of
        Closed Bomb -> n + 1
        Flagged Bomb -> n + 1
        _ -> n

countBomb : Cell -> List Cell -> Cell
countBomb cell cells =
    case cell of
        Closed Empty -> Closed <| Hint <| List.foldl addBombCount 0 cells
        x -> x


initTable : TableSize -> Table
initTable (height, width) =
    List.range 1 height
    |> List.map (\_ ->
        List.range 1 width
        |> List.map (\_ -> Closed Empty)
        |> Array.fromList
    )
    |> Array.fromList


type Point = Inside Height Width | Outside

toPointAbs : TableSize -> Int -> Int -> Point
toPointAbs (height, width) y x =
    if x > 0 && y > 0 && x < width && y < height then
        Inside y x
    else
        Outside
toPoint : Int -> Int -> Point
toPoint = toPointAbs easyTableSize

aroundIndices : List (Int, Int)
aroundIndices =
    List.concatMap (\y -> List.map (\x -> (y, x)) [-1, 0 , 1]) [-1, 0 , 1]

aroundPoints : Point -> List Point
aroundPoints point =
    case point of
        Inside y x ->
            aroundIndices
            |> List.map (\(dy, dx) -> toPoint (y + dy) (x + dx))
        Outside -> []

setBombs : List Point -> Table -> Maybe Table
setBombs points table =
    case points of
        p::ps ->
            case p of
                Inside y x ->
                    Array.get y table
                    |> Maybe.map (Array.set x (Closed Bomb))
                    |> Maybe.map (\row -> Array.set y row table)
                    |> Maybe.andThen (setBombs ps)
                Outside -> Nothing
        [] -> Just table


calcHint : Table -> Point -> Maybe Cell
calcHint table point =
    aroundPoints point
    |> List.foldl (\p mN->
        case (mN, p) of
            (Just n, Inside y x) ->
                Array.get y table
                |> Maybe.andThen (Array.get x)
                |> Maybe.map (\cell ->
                    case cell of
                        Closed Bomb -> n + 1
                        Opened Bomb -> n + 1
                        Flagged Bomb -> n + 1
                        _ -> n
                )
            (_, Outside) -> Nothing
            (Nothing, _) -> Nothing
    ) (Just 0)
    |> Maybe.map (\n -> Closed <| Hint n)

tablePoints : TableSize -> List Point
tablePoints (height, width) =
    List.range 0 (height - 1)
    |> List.concatMap (\y ->
        List.range 0 (width - 1)
        |> List.map (Inside y))

setHints : TableSize -> Table -> Maybe Table
setHints size table =
    tablePoints size
    |> List.foldl (\point mNewTable ->
        case (point, mNewTable) of
            (Inside y x, Just newTable) ->
                calcHint table point
                |> Maybe.andThen (\hint ->
                    Array.get y newTable
                    |> Maybe.map (Array.set x hint)
                    |> Maybe.map (\row -> Array.set y row newTable)
                )
            (Outside, _) -> Nothing
            (_, Nothing) -> Nothing
    ) (Just table)


randomBombPoints : TableSize -> BombCount -> Random.Generator (List Point)
randomBombPoints tableSize bombCount =
    tablePoints tableSize
    |> Random.List.shuffle
    |> Random.map (List.take bombCount)
                

initRandomBombTable : TableSize -> BombCount -> Random.Generator (Maybe Table)
initRandomBombTable tableSize bombCount =
    randomBombPoints tableSize bombCount
    |> Random.map (\points ->
        initTable tableSize
        |> setBombs points
        |> Maybe.andThen (setHints tableSize)
    )


main = text "Hello world!"
