module BoardTests exposing (..)

import List
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, constant)
import Test exposing (..)
import Player exposing (Player, black, white)
import Board exposing (Board, InsertionFailure(..))


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


suite : Test
suite =
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
        , test "suicide" <|
            \_ ->
                let
                    board =
                        boardWith 3 [ ( 2, 1 ), ( 1, 2 ), ( 3, 2 ), ( 2, 3 ) ] []

                    coordinate =
                        ( 2, 2 )

                    player =
                        white
                in
                    case Board.insert coordinate player board of
                        Ok _ ->
                            Expect.fail "should have been suicide"

                        Err reason ->
                            Expect.equal reason Suicide
        ]
