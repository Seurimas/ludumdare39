module World.View exposing (..)

import Math
import Math.Matrix4 as M4
import Math.Vector3 as V3
import Game.TwoD.Camera exposing (..)
import Vector2 exposing (Float2, getX, getY)


type alias ViewableWorld x =
    { x
        | camera : Camera
        , screenSize : Float2
    }


initCamera =
    fixedHeight 16 ( 0, 0 )


initScreen =
    ( 500, 500 )


screenToWorld : ViewableWorld x -> Float2 -> Float2
screenToWorld { camera, screenSize } ( x, y ) =
    let
        gameSize =
            getViewSize screenSize camera

        bottomLeft =
            Vector2.sub (getPosition camera) (Vector2.scale 0.5 gameSize)

        gamePos =
            Vector2.add
                bottomLeft
                ( x / getX screenSize * getX gameSize, ((1 - y / getY screenSize) * getY gameSize) )
    in
        gamePos


getScreenBounds : ViewableWorld x -> Math.Rectangle
getScreenBounds { camera, screenSize } =
    let
        ( width, height ) =
            getViewSize screenSize camera

        bottomLeft =
            Vector2.sub (getPosition camera) (Vector2.scale 0.5 ( width, height ))
    in
        { x = getX bottomLeft
        , y = getY bottomLeft
        , width = width
        , height = height
        }


getScreenCenter : ViewableWorld x -> Float2
getScreenCenter { camera } =
    getPosition camera
