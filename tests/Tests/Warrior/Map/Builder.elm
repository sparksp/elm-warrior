module Tests.Warrior.Map.Builder exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Map.Builder as Builder


all : Test
all =
    describe "Warrior.Internal.Builder"
        [ describe "spawnPoints" spawnPointsTests
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
