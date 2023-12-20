module TableTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Model.Table exposing (..)
import Set
import Array


testInitTable : Test
testInitTable =
    test
        "initTable"
        (\_ ->
            let
                {container} = Model.Table.initTable (2, 2) (Set.fromList [(0, 0), (1, 1)])
                expect =
                    [
                        [(Closed, Bomb), (Closed, Hint 2)],
                        [(Closed, Hint 2), (Closed, Bomb)]
                    ]
                    |> List.map Array.fromList
                    |> Array.fromList
            in
                Expect.equal expect container
        )
