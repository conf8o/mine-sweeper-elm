module Main exposing (..)
import Array exposing (..)
import Browser
import Html exposing (Html)
import Model.Table exposing (Table, initTable)
import Update.Table exposing (open, flag, updateTable)
import View.Table
import Set

type Model =
    TablePage Table

type Msg =
    TableCellClick View.Table.Msg 

update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( TableCellClick (View.Table.LeftClickCell point), TablePage table ) ->
            TablePage <| updateTable open point table
        ( TableCellClick (View.Table.RightClickCell point), TablePage table ) ->
            TablePage <| updateTable flag point table

view : Model -> Html Msg
view (TablePage table) =
    View.Table.view table
    |> Html.map (\m -> TableCellClick m)
    

init : Model
init = TablePage <| initTable (2, 3) <| Set.fromList [(0, 0), (1, 1)]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
