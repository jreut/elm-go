module Board
    exposing
        ( Board
        , Player(..)
        , InsertionFailure(..)
        , insert
        , get
        , square
        )

import Result exposing (andThen)
import Dict exposing (Dict)


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


type alias Coordinate =
    ( Int, Int )


type Player
    = Black
    | White


square : Int -> Board
square size =
    Board { dict = Dict.empty, size = size }


insert : Coordinate -> Player -> Board -> InsertionResult
insert coordinate player (Board { size, dict }) =
    let
        fromDict dict =
            Board { dict = dict, size = size }
    in
        validateCoordinate size coordinate
            |> andThen (\_ -> validateAvailable dict coordinate)
            |> andThen
                (\_ ->
                    Dict.insert coordinate player dict
                        |> fromDict
                        |> Result.Ok
                )


validateCoordinate : Int -> Coordinate -> Result InsertionFailure ()
validateCoordinate size ( x, y ) =
    if
        List.all identity
            [ x > 0
            , x <= size
            , y > 0
            , y <= size
            ]
    then
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


get : Coordinate -> Board -> Maybe Player
get coordinate (Board { dict }) =
    Dict.get coordinate dict
