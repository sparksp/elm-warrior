module Tests.Warrior.Map.Builder exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Internal.Map as Map exposing (Map(..))
import Warrior.Map.Builder as Builder
import Warrior.Map.Test
import Warrior.Map.Tile as Tile


all : Test
all =
    describe "Warrior.Internal.Builder"
        [ describe "spawnPoints" spawnPointsTests
        , describe "withDescription" withDescriptionTests
        , describe "withExitPoint" withExitPointTests
        , describe "withSpawnPoint" withSpawnPointTests
        , describe "withWalledArea" withWalledAreaTests
        ]


spawnPointsTests : List Test
spawnPointsTests =
    [ test "returns a list of all Spawn Point's Coordinates" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withSpawnPoint { x = 1, y = 1 }
                |> Builder.withSpawnPoint { x = 4, y = 4 }
                |> Builder.spawnPoints
                |> Expect.equalLists
                    [ { x = 1, y = 1 }
                    , { x = 4, y = 4 }
                    ]
    , test "ignores any duplicate Spawn Points" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withSpawnPoint { x = 2, y = 3 }
                |> Builder.withSpawnPoint { x = 2, y = 3 }
                |> Builder.spawnPoints
                |> Expect.equalLists [ { x = 2, y = 3 } ]
    ]


withDescriptionTests : List Test
withDescriptionTests =
    [ test "adds description to the map" <|
        \() ->
            Builder.init { rows = 1, columns = 1 }
                |> Builder.withDescription "A test map."
                |> Builder.build
                |> mapDescription
                |> Expect.equal "A test map."
    ]


withExitPointTests : List Test
withExitPointTests =
    [ test "adds an Exit to the map" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withExitPoint { x = 1, y = 0 }
                |> Builder.build
                |> Map.tileAtPosition { x = 1, y = 0 }
                |> Expect.equal Tile.Exit
    , test "does not add an Exit out of bounds" <|
        \() ->
            Builder.init { rows = 2, columns = 2 }
                |> Builder.withExitPoint { x = 0, y = 3 }
                |> Builder.withExitPoint { x = 3, y = 0 }
                |> Builder.withExitPoint { x = 3, y = 3 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty ]
                    , [ Tile.Empty, Tile.Empty ]
                    ]
    ]


withSpawnPointTests : List Test
withSpawnPointTests =
    [ test "adds a SpawnPoint to the map" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withSpawnPoint { x = 3, y = 0 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.SpawnPoint, Tile.Empty ]
                    ]
    , test "does not add a SpawnPoint out of bounds" <|
        \() ->
            Builder.init { rows = 2, columns = 2 }
                |> Builder.withSpawnPoint { x = 3, y = 0 }
                |> Builder.withSpawnPoint { x = 0, y = 3 }
                |> Builder.withSpawnPoint { x = 3, y = 3 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty ]
                    , [ Tile.Empty, Tile.Empty ]
                    ]
    ]


withWalledAreaTests : List Test
withWalledAreaTests =
    [ test "adds a block of Walls to the map" <|
        \() ->
            Builder.init { rows = 4, columns = 4 }
                |> Builder.withWalledArea { x = 1, y = 1 } { x = 2, y = 2 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Empty ]
                    , [ Tile.Empty, Tile.Wall, Tile.Wall, Tile.Empty ]
                    , [ Tile.Empty, Tile.Wall, Tile.Wall, Tile.Empty ]
                    , [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Empty ]
                    ]
    , test "does not add Walls outside of map bounds" <|
        \() ->
            Builder.init { rows = 2, columns = 4 }
                |> Builder.withWalledArea { x = 3, y = 0 } { x = 5, y = 3 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Wall ]
                    , [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Wall ]
                    ]
    ]



--- HELPERS


mapDescription : Map -> String
mapDescription (Map map) =
    map.description
