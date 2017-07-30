module World.Render exposing (..)

import Slime exposing (..)
import World exposing (..)
import Game.TwoD.Render as Render
import Vector2 exposing (sub, angle)
import Math
import Color
import World.View as View
import World.Input as Input
import World.Enemies as Enemies
import Assets.Reference exposing (Sprite(Player), getSprite)


drawPlayer assets { a, b, c } =
    let
        sprite =
            (getSprite Player assets)

        rotation =
            c
    in
        Render.manuallyManagedAnimatedSpriteWithOptions { sprite | position = ( b.x + b.width / 2, b.y + b.height / 2, 0 ), size = ( b.width, b.height ), rotation = rotation }


drawEnemy assets { a, b, c } =
    let
        sprite =
            (getSprite a.sprite assets)

        rotation =
            c

        currentFrame =
            Enemies.getFrame a
    in
        Render.manuallyManagedAnimatedSpriteWithOptions
            { sprite
                | position = ( b.x + b.width / 2, b.y + b.height / 2, 0 )
                , size = ( b.width, b.height )
                , rotation = rotation
                , currentFrame = currentFrame
            }


drawProjectile assets { a } =
    let
        sprite =
            (getSprite a.sprite assets)

        rotation =
            (Math.angleOf a.vel) - pi / 2
    in
        Render.manuallyManagedAnimatedSpriteWithOptions { sprite | position = ( Vector2.getX a.pos, Vector2.getY a.pos, 0 ), size = ( 0.4, 0.65 ), rotation = rotation }


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
            world &. (entities3 player transforms rotations)

        enemieEnts =
            world &. (entities3 enemies transforms rotations)

        projectiles =
            world &. (entities playerProjectiles)
    in
        []
            -- ++ drawMarkers world
            ++
                [ drawReticle world ]
            ++ List.map (drawPlayer world.assets) players
            ++ List.map (drawEnemy world.assets) enemieEnts
            ++ List.map (drawProjectile world.assets) projectiles
            ++ (List.map drawHealthBar enemieEnts |> List.concat)
