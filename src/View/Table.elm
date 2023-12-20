module View.Table exposing (..)
import Model.Table exposing (..)
import Html exposing (text, div, Html)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Array exposing (Array)

type Msg =
    LeftClickCell Point
    | RightClickCell Point


closedCell : Html Msg
closedCell = text ""

hintCell : Int -> Html Msg
hintCell x = text <| String.fromInt x

emptyCell : Html Msg
emptyCell = text "□"


bombCell : Html Msg
bombCell = text "💣"

flaggedCell : Html Msg
flaggedCell = text "🚩"

cellElement : Cell -> Html Msg
cellElement cell =
    case cell of
        ( Closed, _ ) -> closedCell
        ( Opened, Hint x ) -> hintCell x
        ( Opened, Empty ) -> emptyCell
        ( Opened, Bomb ) -> bombCell
        ( Flagged, _ ) -> flaggedCell

cellBackgroundColor : Cell -> String
cellBackgroundColor (cellStatus, _) =
    case cellStatus of
        Closed -> "#A4A4A4"
        Flagged -> "#A4A4A4"
        Opened -> "#FFFFFF"

cellAtrributes : { point: Point, cell: Cell } -> List (Html.Attribute Msg)
cellAtrributes { point, cell } =
    [ style "height" "32px"
    , style "width" "32px"
    , style "text-align" "center"
    , style "vertical-align" "center"
    , style "border" "#000000 1px solid"
    , style "background-color" (cellBackgroundColor cell)
    , Mouse.onClick (\_ -> LeftClickCell point)
    , Mouse.onContextMenu (\_ -> RightClickCell point)]

cellView : { point: Point, cell: Cell } -> Html Msg
cellView props =
    div (cellAtrributes props) [cellElement props.cell]

rowView : { y: Int, row: Array Cell } -> Html Msg
rowView { y, row } =
    row
    |> Array.toList
    |> List.indexedMap (\x cell -> cellView { point = (y, x), cell = cell })
    |> div [ style "display" "flex" ]

view : Table -> Html Msg
view { container } =
    container
    |> Array.toList
    |> List.indexedMap (\y row -> rowView { y = y, row = row })
    |> div []
