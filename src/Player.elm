module Player
    exposing
        ( Player(..)
        , next
        , black
        , white
        )


type Player
    = White
    | Black


black : Player
black =
    Black


white : Player
white =
    White


next : Player -> Player
next player =
    case player of
        White ->
            Black

        Black ->
            White
