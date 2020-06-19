module Tests.Warrior.Internal.History exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior
import Warrior.Direction as Direction
import Warrior.Internal.History as History
import Warrior.Internal.Map exposing (Map)
import Warrior.Internal.Warrior as Player
import Warrior.Map.Builder as Builder


all : Test
all =
    describe "Warrior.Internal.History"
        [ describe "init" initTests
        , describe "previousActions" previousActionsTests
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



--- HELPERS


testPlayer : Player.Warrior
testPlayer =
    Player.spawnHero "Tester" { x = 0, y = 0 }


emptyMap : Map
emptyMap =
    Builder.init { rows = 5, columns = 5 }
        |> Builder.build
