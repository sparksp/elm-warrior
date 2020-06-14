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
        , describe "withPosition" withPositionTests
        , describe "attack" attackTests
        , describe "attackDamage" attackDamageTests
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
            testPlayer
                |> Player.addItem Item.Sword
                |> Player.addItem Item.Potion
                |> Player.inventory
                |> Expect.equalLists [ Item.Potion, Item.Sword ]
    ]


withPositionTests : List Test
withPositionTests =
    [ test "updates the player's position" <|
        \() ->
            let
                coords =
                    { x = 1, y = 2 }
            in
            testPlayer
                |> Player.withPosition coords
                |> Player.position
                |> Expect.equal coords
    ]


attackTests : List Test
attackTests =
    [ test "with no Sword damages the defender by 1" <|
        \() ->
            let
                attacker =
                    testPlayer
            in
            testPlayer
                |> Player.attack attacker
                |> Player.health
                |> Expect.equal 9
    , test "with a Sword damages the defender by 3" <|
        \() ->
            let
                attacker =
                    testPlayer
                        |> Player.addItem Item.Sword
            in
            testPlayer
                |> Player.attack attacker
                |> Player.health
                |> Expect.equal 7
    ]


attackDamageTests : List Test
attackDamageTests =
    [ test "with no Sword damage is 1" <|
        \() ->
            testPlayer
                |> Player.attackDamage
                |> Expect.equal 1
    , test "with a Sword damage is 3" <|
        \() ->
            testPlayer
                |> Player.addItem Item.Sword
                |> Player.attackDamage
                |> Expect.equal 3
    , test "with 2 Swords damage is still 3" <|
        \() ->
            testPlayer
                |> Player.addItem Item.Sword
                |> Player.addItem Item.Sword
                |> Player.attackDamage
                |> Expect.equal 3
    ]


testPlayer : Player.Warrior
testPlayer =
    Player.spawnHero "test" { x = 0, y = 0 }
