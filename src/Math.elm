module Math exposing (..)

import Vector2 exposing (..)


type alias Rectangle =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


center : Rectangle -> Float2
center rect =
    ( rect.x + rect.width / 2, rect.y + rect.height / 2 )


moveRectangle : Float2 -> Rectangle -> Rectangle
moveRectangle ( x, y ) rectangle =
    { rectangle | x = x + rectangle.x, y = y + rectangle.y }


separation : Rectangle -> Rectangle -> Maybe Float2
separation me them =
    let
        totalRadius =
            (me.width + them.width) / 2

        lengthSquared =
            totalRadius * totalRadius

        differenceSquared =
            (distanceSquared (center me) (center them))
    in
        if differenceSquared > lengthSquared || differenceSquared == 0 then
            Nothing
            -- Already separated
        else
            let
                separationAmount =
                    Debug.log "amount" (distance (center me) (center them) - totalRadius)
            in
                Just (Debug.log "Separate!" (scale separationAmount (directionFromTo (center me) (center them))))


segmentCircle : Float2 -> Float2 -> Rectangle -> Bool
segmentCircle ( px, py ) ( vx, vy ) { x, y, width, height } =
    let
        tx =
            x + width / 2 - px

        ty =
            y + height / 2 - py

        nearPoint =
            Debug.log "Near" (project (Debug.log "Target" ( tx, ty )) (Debug.log "Vector" ( vx, vy )))
    in
        (lengthSquared nearPoint <= lengthSquared ( vx, vy ))
            && (distance ( tx, ty ) nearPoint <= width / 2)
