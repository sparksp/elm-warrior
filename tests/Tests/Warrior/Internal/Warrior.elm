module Tests.Warrior.Internal.Warrior exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Internal.Warrior as Player
import Warrior.Item as Item


all : Test
all =
    describe "Internal.Warrior"
        [ describe "spawnHero" spawnHeroTests
        , describe "spawnVillain" spawnVillainTests
        , describe "addItem" addItemTests
        ]


initialHealth : Int
initialHealth =
    10


spawnHeroTests : List Test
spawnHeroTests =
    [ test "returns a Hero with the given id and coords" <|
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
                    , Player.isVillain >> Expect.equal False

                    -- Defaults
                    , Player.health >> Expect.equal initialHealth
                    , Player.maxHealth >> Expect.equal initialHealth
                    , Player.inventory >> List.isEmpty >> Expect.equal True
                    ]
    ]


spawnVillainTests : List Test
spawnVillainTests =
    [ test "returns a Villain with the given id and coords" <|
        \() ->
            let
                name =
                    "evil"

                coords =
                    { x = 1, y = 1 }
            in
            Player.spawnVillain name coords
                |> Expect.all
                    [ Player.id >> Expect.equal name
                    , Player.position >> Expect.equal coords
                    , Player.isVillain >> Expect.equal True
                    , Player.isHero >> Expect.equal False

                    -- Defaults
                    , Player.health >> Expect.equal initialHealth
                    , Player.maxHealth >> Expect.equal initialHealth
                    , Player.inventory >> List.isEmpty >> Expect.equal True
                    ]
    ]


addItemTests : List Test
addItemTests =
    [ test "adds an item to the inventory" <|
        \() ->
            Player.spawnHero "at" { x = 0, y = 0 }
                |> Player.addItem Item.Sword
                |> Player.addItem Item.Potion
                |> Player.inventory
                |> Expect.equalLists [ Item.Potion, Item.Sword ]
    ]
