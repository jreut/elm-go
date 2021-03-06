module Main exposing (main)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Board exposing (Board)
import Player exposing (Player)
import Coordinate exposing (Coordinate)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { board : Board
    , turn : Player
    , message : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    { board = Board.square 19
    , turn = Player.black
    , message = Nothing
    }
        ! []



-- VIEW


view : Model -> Html Msg
view model =
    let
        size =
            Board.size model.board

        messages =
            case model.message of
                Nothing ->
                    []

                Just message ->
                    [ Html.div [] [ Html.text message ]
                    ]
    in
        Html.main_ []
            ([ Html.table []
                [ Html.tbody []
                    (List.map (viewRow model.board size)
                        (List.range 1 size)
                    )
                ]
             , Html.button [ onClick Pass ] [ Html.text "pass" ]
             ]
                ++ messages
            )


viewRow : Board -> Int -> Int -> Html Msg
viewRow board xSize y =
    let
        range =
            List.map (flip Coordinate.fromXandY y) (List.range 1 xSize)
    in
        Html.tr [] (List.map (viewCell board) range)


viewCell : Board -> Coordinate -> Html Msg
viewCell board coordinate =
    let
        occupant =
            Board.get coordinate board

        rendered =
            case occupant of
                Just player ->
                    Html.text <| toString player

                Nothing ->
                    Html.em []
                        [ Html.text <| toString coordinate
                        ]
    in
        Html.td [ onClick (Click coordinate) ] [ rendered ]



-- UPDATE


type Msg
    = Click Coordinate
    | Pass


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click coordinate ->
            case Board.insert coordinate model.turn model.board of
                Ok board ->
                    { model
                        | board = board
                        , turn = Player.next model.turn
                        , message = Nothing
                    }
                        ! []

                Err reason ->
                    { model | message = Just <| toString reason } ! []

        Pass ->
            { model | turn = Player.next model.turn } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
