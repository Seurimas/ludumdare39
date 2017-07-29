module Math exposing (..)

import Vector2 exposing (..)


type alias Rectangle =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


moveRectangle : Float2 -> Rectangle -> Rectangle
moveRectangle ( x, y ) rectangle =
    { rectangle | x = x + rectangle.x, y = y + rectangle.y }
