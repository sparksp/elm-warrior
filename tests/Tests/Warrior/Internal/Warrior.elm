module Tests.Warrior.Internal.Warrior exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Internal.Warrior as Player


all : Test
all =
    describe "Internal.Warrior"
        [ describe "spawnHero" spawnHeroTests
        ]


initialHealth : Int
initialHealth =
    10


spawnHeroTests : List Test
spawnHeroTests =
    [ test "returns a warrior with the given id and coords" <|
        \() ->
            let
                name =
                    "at"

                coords =
                    { x = 0, y = 0 }
            in
            Player.spawnHero name coords
                |> Expect.all
                    [ Player.id >> Expect.equal name
                    , Player.position >> Expect.equal coords
                    , Player.isHero >> Expect.equal True

                    -- Defaults
                    , Player.health >> Expect.equal initialHealth
                    , Player.maxHealth >> Expect.equal initialHealth
                    , Player.inventory >> List.isEmpty >> Expect.equal True
                    ]
    ]
