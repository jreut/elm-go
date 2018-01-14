module Board
    exposing
        ( Board
        , InsertionFailure(..)
        , insert
        , get
        , size
        , square
        )

import Result exposing (andThen)
import Dict exposing (Dict)
import Player exposing (Player)
import Coordinate exposing (Coordinate)


type alias InsertionResult =
    Result InsertionFailure Board


type InsertionFailure
    = Ko
    | Suicide
    | Occupied
    | OutOfBounds


type Board
    = Board
        { dict : Dict Coordinate Player
        , size : Int
        }


square : Int -> Board
square size =
    Board { dict = Dict.empty, size = size }


size : Board -> Int
size (Board { size }) =
    size


insert : Coordinate -> Player -> Board -> InsertionResult
insert coordinate player (Board board) =
    validateCoordinate board.size coordinate
        |> andThen (\_ -> validateAvailable board.dict coordinate)
        |> andThen (\_ -> validateLiberties (Board board) coordinate player)
        |> andThen (\_ -> set coordinate player (Board board) |> Result.Ok)


validateCoordinate : Int -> Coordinate -> Result InsertionFailure ()
validateCoordinate size coordinate =
    if Coordinate.isWithinSquare size coordinate then
        Result.Ok ()
    else
        Result.Err OutOfBounds


validateAvailable : Dict Coordinate Player -> Coordinate -> Result InsertionFailure ()
validateAvailable dict coordinate =
    case Dict.get coordinate dict of
        Nothing ->
            Result.Ok ()

        Just _ ->
            Result.Err Occupied


validateLiberties : Board -> Coordinate -> Player -> Result InsertionFailure ()
validateLiberties board coordinate player =
    let
        inserted =
            set coordinate player board
    in
        case liberties coordinate inserted of
            0 ->
                Result.Err Suicide

            _ ->
                Result.Ok ()


get : Coordinate -> Board -> Maybe Player
get coordinate (Board { dict }) =
    Dict.get coordinate dict


set : Coordinate -> Player -> Board -> Board
set coordinate player (Board board) =
    Board { board | dict = Dict.insert coordinate player board.dict }


liberties : Coordinate -> Board -> Int
liberties ( x, y ) (Board { dict, size }) =
    let
        compass =
            [ ( x + 1, y )
            , ( x - 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            ]

        toCheck =
            List.filter (Coordinate.isWithinSquare size) compass
    in
        toCheck
            |> List.filterMap (\coordinate -> Dict.get coordinate dict)
            |> List.length
            |> ((-) (List.length toCheck))
