module World.Render exposing (..)

import Slime exposing (..)
import World exposing (..)
import Game.TwoD.Render as Render
import Vector2 exposing (sub)
import Math
import Color
import World.View as View
import World.Input as Input


drawPlayer { a, b } =
    Render.shape Render.circle { color = Color.green, position = ( b.x, b.y ), size = ( b.width, b.height ) }


drawEnemy { a, b } =
    Render.shape Render.circle { color = Color.red, position = ( b.x, b.y ), size = ( b.width, b.height ) }


drawProjectile { a } =
    Render.shape Render.circle { color = Color.black, position = ( Vector2.getX a.pos, Vector2.getY a.pos ), size = ( 0.125, 0.125 ) }


drawReticle world =
    let
        ( gameX, gameY ) =
            Input.gameMouse world
    in
        Render.shape Render.circle { color = Color.black, position = ( gameX - 0.25, gameY - 0.25 ), size = ( 0.5, 0.5 ) }


drawHealthBar { a, b } =
    let
        percent =
            a.health / a.maxHealth

        fullWidth =
            b.width

        partWidth =
            percent * b.width

        place =
            ( b.x, b.y + b.height )

        margin =
            0.05

        height =
            0.125
    in
        [ Render.shape Render.rectangle { color = Color.red, position = sub place ( margin, margin ), size = ( fullWidth + margin * 2, height + margin * 2 ) }
        , Render.shape Render.rectangle { color = Color.black, position = place, size = ( fullWidth, height ) }
        , Render.shape Render.rectangle { color = Color.red, position = place, size = ( partWidth, height ) }
        ]


drawMarkers world =
    let
        screen =
            View.getScreenBounds world

        markers =
            List.range (floor screen.x) (ceiling (screen.x + screen.width))
                |> List.map
                    (\x ->
                        List.range (floor screen.y) (ceiling (screen.y + screen.height))
                            |> List.map (\y -> ( x, y ))
                    )
                |> List.concat

        drawMarker ( x, y ) =
            Render.shape Render.circle { color = Color.blue, position = ( toFloat x, toFloat y ), size = ( 0.25, 0.25 ) }
    in
        markers
            |> List.map drawMarker


renderWorld : World -> List Render.Renderable
renderWorld world =
    let
        players =
            world &. (entities2 player transforms)

        enemieEnts =
            world &. (entities2 enemies transforms)

        projectiles =
            world &. (entities playerProjectiles)
    in
        []
            ++ drawMarkers world
            ++ [ drawReticle world ]
            ++ List.map drawPlayer players
            ++ List.map drawEnemy enemieEnts
            ++ List.map drawProjectile projectiles
            ++ (List.map drawHealthBar enemieEnts |> List.concat)
