module Tests.Warrior.Internal.Map exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Direction as Direction
import Warrior.Internal.Map as Map
import Warrior.Internal.Warrior as Player
import Warrior.Item as Item
import Warrior.Map.Builder as Builder
import Warrior.Map.Tile as Tile
import Warrior.Npc.Dummy as Dummy


all : Test
all =
    describe "Warrior.Internal.Map"
        [ describe "coordinateFrom" coordinateFromTests
        , describe "lookDown" lookDownTests
        , describe "tileAtPosition" tileAtPositionTests
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


tileAtPositionTests : List Test
tileAtPositionTests =
    [ describe "SpawnPoint"
        [ test "when the coordinate is a Spawn Point" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withSpawnPoint { x = 0, y = 0 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 0, y = 0 }
                    |> Expect.equal Tile.SpawnPoint
        ]
    , describe "Exit"
        [ test "when the coordinate is an Exit" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withExitPoint { x = 4, y = 0 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 4, y = 0 }
                    |> Expect.equal Tile.Exit
        ]
    , describe "Empty"
        [ test "when the coordinate is an Empty tile with no item or NPC" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 2, y = 0 }
                    |> Expect.equal Tile.Empty
        , test "when the coordinate is an Empty tile with a dead NPC" <|
            \() ->
                let
                    dummy : Player.Warrior
                    dummy =
                        Player.spawnVillain "Dummy" { x = 4, y = 0 }
                            |> injurePlayerBy 10
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withNpc "Dummy" { x = 4, y = 0 } Dummy.takeTurn
                    |> Builder.build
                    |> Map.setNpcs [ dummy ]
                    |> Map.tileAtPosition { x = 4, y = 0 }
                    |> Expect.equal Tile.Empty
        ]
    , describe "Item"
        [ test "when the coordinate is an Empty tile with an Item and no NPC" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withItem { x = 1, y = 0 } Item.Potion
                    |> Builder.build
                    |> Map.tileAtPosition { x = 1, y = 0 }
                    |> Expect.equal (Tile.Item Item.Potion)
        ]
    , describe "Wall"
        [ test "when the coordinate is a Wall" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withWalledArea { x = 1, y = 0 } { x = 3, y = 0 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 2, y = 0 }
                    |> Expect.equal Tile.Wall
        , test "when the coordinate is not on the map" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 1, y = 9 }
                    |> Expect.equal Tile.Wall
        ]
    , describe "Warrior"
        [ test "when the coordinate is an Empty tile with a Warrior" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withNpc "Dummy" { x = 3, y = 0 } Dummy.takeTurn
                    |> Builder.build
                    |> Map.tileAtPosition { x = 3, y = 0 }
                    |> Expect.equal (Tile.Warrior "Dummy")
        , test "when the coordinate is an Empty tile with an Item and a Warrior" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withItem { x = 4, y = 0 } Item.Sword
                    |> Builder.withNpc "Dummy" { x = 4, y = 0 } Dummy.takeTurn
                    |> Builder.build
                    |> Map.tileAtPosition { x = 4, y = 0 }
                    |> Expect.equal (Tile.Warrior "Dummy")
        ]
    ]



--- HELPERS


injurePlayerBy : Int -> Player.Warrior -> Player.Warrior
injurePlayerBy damage player =
    let
        attacker : Player.Warrior
        attacker =
            Player.spawnVillain "Attacker" { x = 0, y = 0 }
    in
    List.repeat damage ()
        |> List.foldl (\() -> Player.attack attacker) player
