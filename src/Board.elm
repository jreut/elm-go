module Board
    exposing
        ( Board
        , InsertionResult
        , InsertionFailure(..)
        , insert
        , get
        , set
        , size
        , square
        , toString
        )

import Result exposing (andThen)
import Dict exposing (Dict)
import Set exposing (Set)
import Player exposing (Player)
import Coordinate exposing (Coordinate)


type alias InsertionResult =
    Result InsertionFailure Board


type InsertionFailure
    = Ko
    | Suicide
    | Occupied
    | OutOfBounds
    | Unspecified String


type Board
    = Board
        { dict : Dict Coordinate Player
        , size : Int
        }



-- TODO: implement Nat


type alias Nat =
    Int


type alias Group =
    { coordinates : Set Coordinate
    , player : Player
    , liberties : Set Coordinate
    }


square : Int -> Board
square size =
    Board { dict = Dict.empty, size = size }


size : Board -> Int
size (Board { size }) =
    size


insert : Coordinate -> Player -> Board -> InsertionResult
insert coordinate player (Board board) =
    validateCoordinate coordinate (Board board)
        |> andThen (\_ -> validateAvailable board.dict coordinate)
        |> andThen (\_ -> validateLiberties (Board board) coordinate player)
        |> andThen (\_ -> set coordinate player (Board board) |> Result.Ok)


validateCoordinate : Coordinate -> Board -> Result InsertionFailure ()
validateCoordinate coordinate board =
    if isInBounds board coordinate then
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
        case group coordinate inserted of
            Nothing ->
                Result.Err <| Unspecified ((toString_ coordinate) ++ "not in a group")

            Just group_ ->
                if Set.size (group_.liberties) == 0 then
                    Result.Err Suicide
                else
                    Result.Ok ()


get : Coordinate -> Board -> Maybe Player
get coordinate (Board { dict }) =
    Dict.get coordinate dict


set : Coordinate -> Player -> Board -> Board
set coordinate player (Board board) =
    Board { board | dict = Dict.insert coordinate player board.dict }


group : Coordinate -> Board -> Maybe Group
group coordinate board =
    get coordinate board
        |> Maybe.map
            (\player ->
                groupRec coordinate
                    board
                    { player = player
                    , liberties = Set.empty
                    , coordinates = Set.empty
                    }
            )


groupRec : Coordinate -> Board -> Group -> Group
groupRec ( x, y ) board group =
    let
        neighbors =
            [ ( x + 1, y )
            , ( x - 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            ]

        toCheck =
            neighbors
                |> List.filter (isInBounds board)
                |> List.filter (not << flip Set.member group.coordinates)

        checker coordinate group =
            case get coordinate board of
                Nothing ->
                    -- liberty
                    { group | liberties = Set.insert coordinate group.liberties }

                Just player ->
                    if player == group.player then
                        -- extend group
                        groupRec
                            coordinate
                            board
                            { group
                                | coordinates = Set.insert coordinate group.coordinates
                            }
                    else
                        -- edge of group
                        group
    in
        List.foldl checker group toCheck


isInBounds : Board -> Coordinate -> Bool
isInBounds (Board { size }) coordinate =
    Coordinate.isWithinSquare size coordinate


toString_ =
    Basics.toString


toString : Board -> String
toString (Board { size, dict }) =
    let
        range =
            List.range 1 size

        row : Int -> String
        row y =
            range
                |> List.map
                    (\x ->
                        case Dict.get ( x, y ) dict of
                            Nothing ->
                                '-'

                            Just player ->
                                Player.toChar player
                    )
                |> String.fromList
                |> (flip (++) "\n")

        rows : String
        rows =
            List.foldr ((++) << row) "" range
    in
        (toString_ size) ++ "\n" ++ rows
