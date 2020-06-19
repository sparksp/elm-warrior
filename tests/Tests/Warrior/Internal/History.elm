module Tests.Warrior.Internal.History exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior
import Warrior.Direction as Direction
import Warrior.Internal.History as History exposing (History)
import Warrior.Internal.Map exposing (Map)
import Warrior.Internal.Warrior as Player
import Warrior.Item as Item
import Warrior.Map.Builder as Builder


all : Test
all =
    describe "Warrior.Internal.History"
        [ describe "init" initTests

        -- `record` is tested by its use in the following tests
        , describe "previousActions" previousActionsTests
        , describe "previousStates" previousStatesTests
        ]


initTests : List Test
initTests =
    {-
       It's impossible to test that `init` creates an empty History because all
       the other history functions filter the history in some way. The best we
       can do is test that it returns a History value, and rely on other tests
       picking up any problems.
    -}
    [ test "creates an empty history" <|
        \() ->
            let
                player : Player.Warrior
                player =
                    testPlayer
            in
            History.init
                |> Expect.all
                    [ History.previousActions player >> Expect.equal []
                    , History.previousStates player >> Expect.equal []
                    , History.roundsPlayed >> Expect.equal 0
                    ]
    ]


previousActionsTests : List Test
previousActionsTests =
    [ test "with no actions is an empty list" <|
        \() ->
            History.init
                |> History.previousActions testPlayer
                |> Expect.equal []
    , test "only gets actions for the given player with most recent first" <|
        \() ->
            let
                phill : Player.Warrior
                phill =
                    Player.spawnHero "Phill" { x = 0, y = 0 }

                robin : Player.Warrior
                robin =
                    Player.spawnHero "Robin" { x = 4, y = 4 }

                map : Map
                map =
                    emptyMap
            in
            History.init
                |> History.record robin map (Warrior.Move Direction.Left)
                |> History.record phill map (Warrior.Move Direction.Right)
                |> History.record (robin |> Player.withPosition { x = 3, y = 4 }) map (Warrior.Move Direction.Up)
                |> History.record (phill |> Player.withPosition { x = 1, y = 0 }) map (Warrior.Move Direction.Down)
                |> History.record (robin |> Player.withPosition { x = 3, y = 3 }) map Warrior.Wait
                |> Expect.all
                    [ History.previousActions phill
                        >> Expect.equalLists
                            [ Warrior.Move Direction.Down
                            , Warrior.Move Direction.Right
                            ]
                    , History.previousActions robin
                        >> Expect.equalLists
                            [ Warrior.Wait
                            , Warrior.Move Direction.Up
                            , Warrior.Move Direction.Left
                            ]
                    ]
    ]


previousStatesTests : List Test
previousStatesTests =
    [ test "with no previous states is empty" <|
        \() ->
            History.init
                |> History.previousStates testPlayer
                |> Expect.equal []
    , test "gets player and map state with most recent first" <|
        \() ->
            let
                phill : Player.Warrior
                phill =
                    Player.spawnHero "Phill" { x = 0, y = 0 }

                history : History
                history =
                    ( phill, straightMapWithPotion, History.init )
                        |> step
                            ( Player.withPosition { x = 1, y = 0 }
                            , identity
                            , Warrior.Move Direction.Right
                            )
                        |> step
                            ( Player.addItem Item.Potion
                            , \_ -> straightMapWithoutPotion
                            , Warrior.Pickup
                            )
                        |> step
                            ( identity
                            , identity
                            , Warrior.Wait
                            )
                        |> getHistory
            in
            History.previousStates phill history
                |> Expect.equalLists
                    [ ( phill
                            |> Player.withPosition { x = 1, y = 0 }
                            |> Player.addItem Item.Potion
                      , straightMapWithoutPotion
                      )
                    , ( phill
                            |> Player.withPosition { x = 1, y = 0 }
                      , straightMapWithPotion
                      )
                    , ( phill
                      , straightMapWithPotion
                      )
                    ]
    , test "only gets the given player's history" <|
        \() ->
            let
                phill : Player.Warrior
                phill =
                    Player.spawnHero "Phill" { x = 0, y = 0 }

                robin : Player.Warrior
                robin =
                    Player.spawnHero "Robin" { x = 4, y = 4 }

                map : Map
                map =
                    emptyMap
            in
            History.init
                |> History.record robin map (Warrior.Move Direction.Left)
                |> History.record phill map (Warrior.Move Direction.Right)
                |> History.record (robin |> Player.withPosition { x = 3, y = 4 }) map (Warrior.Move Direction.Up)
                |> History.record (phill |> Player.withPosition { x = 1, y = 0 }) map (Warrior.Move Direction.Down)
                |> History.record (robin |> Player.withPosition { x = 3, y = 3 }) map Warrior.Wait
                |> Expect.all
                    [ History.previousStates phill
                        >> Expect.equalLists
                            [ ( phill |> Player.withPosition { x = 1, y = 0 }, map )
                            , ( phill, map )
                            ]
                    , History.previousStates robin
                        >> Expect.equalLists
                            [ ( robin |> Player.withPosition { x = 3, y = 3 }, map )
                            , ( robin |> Player.withPosition { x = 3, y = 4 }, map )
                            , ( robin, map )
                            ]
                    ]
    ]



--- HELPERS


testPlayer : Player.Warrior
testPlayer =
    Player.spawnHero "Tester" { x = 0, y = 0 }


emptyMap : Map
emptyMap =
    Builder.init { rows = 5, columns = 5 }
        |> Builder.build


straightMapWithoutPotion : Map
straightMapWithoutPotion =
    straightMapTemplate
        |> Builder.build


straightMapWithPotion : Map
straightMapWithPotion =
    straightMapTemplate
        |> Builder.withItem { x = 1, y = 0 } Item.Potion
        |> Builder.build


straightMapTemplate : Builder.Template
straightMapTemplate =
    Builder.init { rows = 1, columns = 5 }
        |> Builder.withSpawnPoint { x = 0, y = 0 }
        |> Builder.withExitPoint { x = 4, y = 0 }


step :
    ( Player.Warrior -> Player.Warrior, Map -> Map, Warrior.Action )
    -> ( Player.Warrior, Map, History )
    -> ( Player.Warrior, Map, History )
step ( mapPlayer, mapMap, action ) ( player, map, history ) =
    ( mapPlayer player
    , mapMap map
    , History.record player map action history
    )


getHistory : ( Player.Warrior, Map, History ) -> History
getHistory ( _, _, history ) =
    history
