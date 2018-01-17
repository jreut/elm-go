module BoardTests exposing (..)

import List
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, constant)
import Test exposing (..)
import Player exposing (Player, black, white)
import Board exposing (Board, InsertionResult, InsertionFailure(..))
import Parser exposing (parse)


tinyBoard : Board
tinyBoard =
    Board.square 3


boardWith : Int -> List ( Int, Int ) -> List ( Int, Int ) -> Board
boardWith size blacks whites =
    let
        fold player coordinates board =
            List.foldl
                (\coordinate board_ ->
                    case Board.insert coordinate player board_ of
                        Ok board__ ->
                            board__

                        Err _ ->
                            Debug.crash ("Problem with " ++ (toString coordinate) ++ " " ++ (toString player))
                )
                board
                coordinates
    in
        Board.square size |> fold black blacks |> fold white whites


inBounds : Int -> Fuzzer Int
inBounds size =
    intRange 1 size


outOfBounds : Int -> Fuzzer Int
outOfBounds size =
    Fuzz.oneOf
        [ constant 0
        , constant (size + 1)
        ]


fuzzPlayer : Fuzzer Player
fuzzPlayer =
    Fuzz.oneOf <| List.map constant [ white, black ]


insertionTest : String -> String -> ( Int, Int ) -> Player -> (InsertionResult -> Expectation) -> Test
insertionTest description boardString coordinate player expectation =
    test description <|
        \_ ->
            parse boardString
                |> Result.map (\board -> expectation <| Board.insert coordinate player board)
                |> Result.withDefault (Expect.fail <| "parse error on " ++ boardString)


suicideTest : String -> String -> ( Int, Int ) -> Player -> Test
suicideTest description boardString coordinate player =
    insertionTest ("suicide: " ++ description)
        boardString
        coordinate
        player
        (\result ->
            case result of
                Ok _ ->
                    Expect.fail "should have been suicide"

                Err reason ->
                    Expect.equal reason Suicide
        )


validTest : String -> String -> ( Int, Int ) -> Player -> Test
validTest description boardString coordinate player =
    insertionTest ("valid: " ++ description)
        boardString
        coordinate
        player
        (\result ->
            case result of
                Ok board ->
                    case Board.get coordinate board of
                        Just player_ ->
                            Expect.equal player player_

                        Nothing ->
                            Expect.fail ((toString coordinate) ++ " should have had " ++ (toString player) ++ " instead of empty")

                Err reason ->
                    Expect.fail ("should have been valid instead of " ++ (toString reason))
        )


moveTest : String -> String -> ( Int, Int ) -> Player -> String -> Test
moveTest description beforeBoard coordinate player afterBoard =
    insertionTest description
        beforeBoard
        coordinate
        player
        (\result ->
            case result of
                Ok board ->
                    Expect.equal afterBoard <| Board.toString board

                Err reason ->
                    Expect.fail ("should have been valid instead of " ++ (toString reason))
        )


insertTests : Test
insertTests =
    describe "Board.insert"
        [ fuzz2 (inBounds 3) (inBounds 3) "in an empty board" <|
            \x y ->
                let
                    coordinate =
                        ( x, y )

                    player =
                        black

                    result =
                        Board.insert coordinate player tinyBoard
                in
                    case result of
                        Ok board ->
                            case Board.get coordinate board of
                                Nothing ->
                                    Expect.fail "should be occupied"

                                Just insertedPlayer ->
                                    Expect.equal insertedPlayer player

                        Err _ ->
                            Expect.fail "should have inserted successfully"
        , fuzz2 (inBounds 3) (inBounds 3) "placing on an occupied coordinate" <|
            \x y ->
                let
                    coordinate =
                        ( x, y )

                    player =
                        black

                    board =
                        case Board.insert coordinate player tinyBoard of
                            Ok b ->
                                b

                            Err reason ->
                                Debug.crash <| toString reason

                    result =
                        Board.insert coordinate player board
                in
                    case result of
                        Ok _ ->
                            Expect.fail "should have failed"

                        Err reason ->
                            Expect.equal reason Occupied
        , fuzz3 (outOfBounds 3) (outOfBounds 3) fuzzPlayer "fails on out of bounds" <|
            \x y player ->
                let
                    coordinate =
                        ( x, y )
                in
                    case Board.insert coordinate player tinyBoard of
                        Ok _ ->
                            Expect.fail "should have out of bounds"

                        Err reason ->
                            Expect.equal reason OutOfBounds
        , suicideTest "four opponent neighbors"
            """3
-B-
B-B
-B-
"""
            ( 2, 2 )
            white
        , suicideTest "on edge"
            """3
B--
-B-
B--
"""
            ( 1, 2 )
            white
        , validTest "with a single liberty left"
            """3
B--
-B-
W--
"""
            ( 1, 2 )
            white
        , validTest "shared liberties"
            """3
-BW
WBW
-B-
"""
            ( 1, 1 )
            white
        , moveTest "normal move"
            """3
---
B-B
-B-
"""
            ( 2, 2 )
            white
            """3
---
BWB
-B-
"""
        , skip <|
            moveTest "capture in the corner"
                """3
WB-
---
---
"""
                ( 1, 2 )
                black
                """3
-B-
B--
---
"""
        , skip <|
            moveTest "capture with liberty race"
                """3
-BB
WWB
WWB
"""
                ( 1, 1 )
                black
                """3
BBB
--B
--B
"""
        ]


toStringTests : Test
toStringTests =
    describe "Board.toString"
        [ test "an empty board" <|
            \_ ->
                let
                    board =
                        Board.square 3

                    expected =
                        "3\n---\n---\n---\n"
                in
                    Expect.equal expected <|
                        Board.toString board
        , test "a board with some stuff" <|
            \_ ->
                let
                    board =
                        boardWith 4 [ ( 1, 1 ) ] [ ( 2, 2 ) ]

                    expected =
                        "4\nB---\n-W--\n----\n----\n"
                in
                    Expect.equal expected <| Board.toString board
        ]
