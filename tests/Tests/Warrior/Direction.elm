module Tests.Warrior.Direction exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Direction as Direction


all : Test
all =
    describe "Warrior.Direction"
        [ describe "toString" toStringTests
        ]


toStringTests : List Test
toStringTests =
    [ test "formats Left" <|
        \() ->
            Direction.toString Direction.Left
                |> Expect.equal "Left"
    , test "formats Up" <|
        \() ->
            Direction.toString Direction.Up
                |> Expect.equal "Up"
    , test "formats Right" <|
        \() ->
            Direction.toString Direction.Right
                |> Expect.equal "Right"
    , test "formats Down" <|
        \() ->
            Direction.toString Direction.Down
                |> Expect.equal "Down"
    ]
