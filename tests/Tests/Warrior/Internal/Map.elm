module Tests.Warrior.Internal.Map exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Direction as Direction
import Warrior.Internal.Map as Map
import Warrior.Internal.Warrior as Player
import Warrior.Item as Item
import Warrior.Map.Builder as Builder
import Warrior.Map.Tile as Tile


all : Test
all =
    describe "Warrior.Internal.Map"
        [ describe "coordinateFrom" coordinateFromTests
        , describe "lookDown" lookDownTests
        ]


coordinateFromTests : List Test
coordinateFromTests =
    [ test "gets coordinate to the Left" <|
        \() ->
            { x = 2, y = 2 }
                |> Map.coordinateFrom Direction.Left
                |> Expect.equal { x = 1, y = 2 }
    , test "gets coordinate to the Right" <|
        \() ->
            { x = 2, y = 2 }
                |> Map.coordinateFrom Direction.Right
                |> Expect.equal { x = 3, y = 2 }
    , test "gets coordinate above (Up)" <|
        \() ->
            { x = 2, y = 2 }
                |> Map.coordinateFrom Direction.Up
                |> Expect.equal { x = 2, y = 1 }
    , test "gets coordinate below (Down)" <|
        \() ->
            { x = 2, y = 2 }
                |> Map.coordinateFrom Direction.Down
                |> Expect.equal { x = 2, y = 3 }
    ]


lookDownTests : List Test
lookDownTests =
    [ describe "SpawnPoint"
        [ test "when the player is on a Spawn Point" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 0, y = 0 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withSpawnPoint { x = 0, y = 0 }
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal Tile.SpawnPoint
        ]
    , describe "Exit"
        [ test "when the player is on an Exit" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 4, y = 0 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withExitPoint { x = 4, y = 0 }
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal Tile.Exit
        ]
    , describe "Empty"
        [ test "when player is on an Empty tile with no items" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 2, y = 0 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal Tile.Empty
        ]
    , describe "Item"
        [ test "when player is on an Empty tile with an Item" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 1, y = 0 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withItem { x = 1, y = 0 } Item.Potion
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal (Tile.Item Item.Potion)
        ]
    , describe "Wall"
        [ test "when player is not on the map" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 1, y = 9 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal Tile.Wall
        ]
    ]
