module Tests.Warrior.Map.Progression exposing (all)

import Expect
import Html.Attributes exposing (name)
import Test exposing (Test, describe, test)
import Warrior.Internal.History as History exposing (History)
import Warrior.Internal.Map exposing (Map)
import Warrior.Internal.Warrior as Player
import Warrior.Map.Builder as Builder
import Warrior.Map.Progression as Progression
import Warrior.Npc.Dummy as Dummy


all : Test
all =
    describe "Warrior.Map.Progression"
        [ describe "lastWarriorStanding" lastWarriorStandingTests
        , describe "reachExitPoint" reachExitPointTests
        , describe "withRoundLimit" withRoundLimitTests
        ]


lastWarriorStandingTests : List Test
lastWarriorStandingTests =
    [ describe "Advance"
        [ test "with 1 remaining player" <|
            \() ->
                let
                    players : List Player.Warrior
                    players =
                        [ Player.spawnHero "Phill" { x = 0, y = 0 }
                        ]
                in
                Progression.lastWarriorStanding players emptyMap emptyHistory
                    |> Expect.equal (Progression.Advance players)
        ]
    , describe "GameOver"
        [ test "with no remaining players" <|
            \() ->
                let
                    players : List Player.Warrior
                    players =
                        []
                in
                Progression.lastWarriorStanding players emptyMap emptyHistory
                    |> Expect.equal Progression.GameOver
        ]
    , describe "Undecided"
        [ test "with more than one player remaining" <|
            \() ->
                let
                    players : List Player.Warrior
                    players =
                        [ Player.spawnHero "Robin" { x = 0, y = 0 }
                        , Player.spawnHero "Phill" { x = 1, y = 0 }
                        ]
                in
                Progression.lastWarriorStanding players emptyMap emptyHistory
                    |> Expect.equal Progression.Undecided
        ]
    ]


reachExitPointTests : List Test
reachExitPointTests =
    [ describe "Advance"
        [ test "with a player on an exit all players advance" <|
            \() ->
                let
                    map : Map
                    map =
                        Builder.init { rows = 5, columns = 5 }
                            |> Builder.withExitPoint { x = 0, y = 0 }
                            |> Builder.withNpc "Evan" { x = 3, y = 3 } Dummy.takeTurn
                            |> Builder.build

                    players : List Player.Warrior
                    players =
                        [ Player.spawnHero "Phill" { x = 0, y = 0 }
                        , Player.spawnHero "Robin" { x = 1, y = 1 }
                        ]
                in
                Progression.reachExitPoint players map emptyHistory
                    |> Expect.equal (Progression.Advance players)
        ]
    , describe "GameOver"
        [ test "with no players" <|
            \() ->
                let
                    players : List Player.Warrior
                    players =
                        []
                in
                Progression.reachExitPoint players emptyMap emptyHistory
                    |> Expect.equal Progression.GameOver
        , test "when all players are dead" <|
            \() ->
                let
                    players : List Player.Warrior
                    players =
                        [ deadPlayer "Phill"
                        , deadPlayer "Robin"
                        ]
                in
                Progression.reachExitPoint players emptyMap emptyHistory
                    |> Expect.equal Progression.GameOver
        ]
    , describe "Undecided"
        [ test "with no players on exit and at least 1 player alive" <|
            \() ->
                let
                    players : List Player.Warrior
                    players =
                        [ Player.spawnHero "Phill" { x = 0, y = 0 }
                        ]
                in
                Progression.reachExitPoint players emptyMap emptyHistory
                    |> Expect.equal Progression.Undecided
        ]
    ]


withRoundLimitTests : List Test
withRoundLimitTests =
    -- TODO Figure out how to create a History with a number of rounds
    -- FIXME I'm not sure if the logic of History.roundsPlayed is broken
    [ Test.todo "is GameOver when the number of rounds is exceeded"
    ]



--- HELPERS


deadPlayer : String -> Player.Warrior
deadPlayer name =
    Player.spawnHero name { x = 0, y = 0 }
        |> injurePlayerUntilHealthIs 0


injurePlayerUntilHealthIs : Int -> Player.Warrior -> Player.Warrior
injurePlayerUntilHealthIs health player =
    if Player.health player > health then
        let
            attacker : Player.Warrior
            attacker =
                Player.spawnHero "attacker" { x = 1, y = 0 }
        in
        injurePlayerUntilHealthIs health (Player.attack attacker player)

    else
        player


emptyMap : Map
emptyMap =
    Builder.init { rows = 1, columns = 5 }
        |> Builder.build


emptyHistory : History
emptyHistory =
    History.init
