module Tests.Warrior.Internal.History exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Internal.History as History
import Warrior.Internal.Warrior as Player


all : Test
all =
    describe "Warrior.Internal.History"
        [ describe "init" initTests
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



--- HELPERS


testPlayer : Player.Warrior
testPlayer =
    Player.spawnHero "Tester" { x = 0, y = 0 }
