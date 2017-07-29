module World.Render exposing (..)

import Slime exposing (..)
import World exposing (..)
import Game.TwoD.Render as Render
import Vector2 exposing (sub)
import Math
import Color
import World.View as View
import World.Input as Input


renderWorld : World -> List Render.Renderable
renderWorld world =
    let
        players =
            world &. (entities2 player transforms)

        enemieEnts =
            world &. (entities2 enemies transforms)

        drawPlayer { a, b } =
            Render.shape Render.circle { color = Color.green, position = ( b.x, b.y ), size = ( b.width, b.height ) }

        drawEnemy { a, b } =
            Render.shape Render.circle { color = Color.red, position = ( b.x, b.y ), size = ( b.width, b.height ) }

        drawReticle =
            let
                ( gameX, gameY ) =
                    Input.gameMouse world
            in
                Render.shape Render.circle { color = Color.black, position = ( gameX - 0.25, gameY - 0.25 ), size = ( 0.5, 0.5 ) }

        screen =
            View.getScreenBounds world

        drawMarker ( x, y ) =
            Render.shape Render.circle { color = Color.blue, position = ( toFloat x, toFloat y ), size = ( 0.25, 0.25 ) }

        markers =
            List.range (floor screen.x) (ceiling (screen.x + screen.width))
                |> List.map
                    (\x ->
                        List.range (floor screen.y) (ceiling (screen.y + screen.height))
                            |> List.map (\y -> ( x, y ))
                    )
                |> List.concat

        drawMarkers =
            markers
                |> List.map drawMarker
    in
        List.map drawPlayer players
            ++ List.map drawEnemy enemieEnts
            ++ [ drawReticle ]
            ++ drawMarkers
