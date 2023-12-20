module Update.Table exposing (..)
import Model.Table exposing (..)

open : Cell -> Cell
open cell =
    case cell of
        ( Closed, content ) -> ( Opened, content )
        ( Flagged, content ) -> ( Flagged, content )
        _ -> cell

flag : Cell -> Cell
flag cell =
    case cell of
        ( Closed, content ) -> ( Flagged, content )
        ( Flagged, content ) -> ( Closed, content )
        _ -> cell

switchCell : (Cell -> Cell) -> Point -> Table -> Table
switchCell f point table =
    Model.Table.get point table
    |> Maybe.map f
    |> Maybe.map (\cell -> Model.Table.set point cell table)
    |> Maybe.withDefault table

updateTable : (Cell -> Cell) -> Point -> Table -> Table
updateTable = switchCell
