module Main exposing (main)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Dict exposing (Dict)


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
    , size : Int
    , turn : Player
    }


type alias Board =
    Dict Coordinate Player


type alias Coordinate =
    ( Int, Int )


type Player
    = White
    | Black


init : ( Model, Cmd Msg )
init =
    { board = Dict.empty
    , size = 19
    , turn = Black
    }
        ! []



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.table []
            [ Html.tbody []
                (List.map (viewRow model.board model.size)
                    (List.range 1 model.size)
                )
            ]
        , Html.button [ onClick Pass ] [ Html.text "pass" ]
        ]


viewRow : Board -> Int -> Int -> Html Msg
viewRow board xSize y =
    let
        range =
            List.map (\x -> ( x, y )) (List.range 1 xSize)
    in
        Html.tr [] (List.map (viewCell board) range)


viewCell : Board -> Coordinate -> Html Msg
viewCell board coordinate =
    let
        occupant =
            Dict.get coordinate board

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
    let
        board =
            case msg of
                Click coordinate ->
                    Dict.insert coordinate model.turn model.board

                Pass ->
                    model.board

        turn =
            case model.turn of
                White ->
                    Black

                Black ->
                    White
    in
        { model | board = board, turn = turn } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
