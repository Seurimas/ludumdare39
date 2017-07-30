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
import Assets.Loading exposing (Assets)
import World.Components exposing (Player)


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


drawParticle assets { a } =
    let
        sprite =
            (getSprite a.sprite assets)
    in
        Render.manuallyManagedAnimatedSpriteWithOptions
            { sprite
                | position = ( Vector2.getX a.pos, Vector2.getY a.pos, 0 )
                , size = a.size
                , currentFrame = sprite.numberOfFrames - floor (toFloat sprite.numberOfFrames * a.lifeLeft / a.maxLife)
            }


drawReticle world =
    let
        ( gameX, gameY ) =
            Input.gameMouse world
    in
        Render.shape Render.circle { color = Color.black, position = ( gameX - 0.25, gameY - 0.25 ), size = ( 0.5, 0.5 ) }


progressBar (( x, y ) as position) ( width, height ) percent color =
    let
        partWidth =
            percent * width

        margin =
            0.05
    in
        [ Render.shape Render.rectangle { color = color, position = sub position ( margin, margin ), size = ( width + margin * 2, height + margin * 2 ) }
        , Render.shape Render.rectangle { color = Color.black, position = position, size = ( width, height ) }
        , Render.shape Render.rectangle { color = color, position = position, size = ( partWidth, height ) }
        ]


drawHealthBar { a, b } =
    let
        percent =
            a.health / a.maxHealth

        fullWidth =
            b.width

        place =
            ( b.x, b.y + b.height )

        height =
            0.125
    in
        progressBar place ( fullWidth, height ) percent Color.red


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


drawBackground : Maybe Assets -> World -> List Render.Renderable
drawBackground assets world =
    let
        screen =
            View.getScreenBounds world

        texture =
            assets |> Maybe.map .spriteMap

        bl =
            ( 1, 512 - 63 )
                |> Vector2.scale (1 / 512)

        tr =
            ( 63, 511 )
                |> Vector2.scale (1 / 512)

        tile x y =
            Render.animatedSprite
                { texture = texture
                , bottomLeft = bl
                , topRight = tr
                , size = ( 4, 4 )
                , position = ( x, y )
                , numberOfFrames = 1
                , duration = 1
                }

        leftX =
            floor (screen.x / 4)

        bottomY =
            floor (screen.y / 4)

        countX =
            ceiling (screen.width / 4)

        countY =
            ceiling (screen.height / 4)

        tiles =
            List.range leftX (leftX + countX)
                |> List.map
                    (\xi ->
                        List.range bottomY (bottomY + countY)
                            |> List.map
                                (\yi ->
                                    tile (toFloat xi * 4) (toFloat yi * 4)
                                )
                    )
                |> List.concat
    in
        tiles


drawUi : World -> Entity3 Player x y -> List Render.Renderable
drawUi world { a } =
    let
        screen =
            View.getScreenBounds world

        spellIconSize =
            ( 1, 1 )

        spellIconPosition =
            ( screen.x + 0.125, screen.y + screen.height - 1.125 )

        spellIconSprite =
            getSprite a.spell.icon world.assets

        spellBarSize =
            ( 4, 0.75 )

        spellBarPosition =
            ( screen.x + 1.25, screen.y + screen.height - 1 )

        spellBarProgress =
            if a.spell.castsLeft == 0 then
                0
            else
                (toFloat a.spell.castsLeft) / (toFloat a.spell.maxCasts)

        healthBarSize =
            ( 5, 0.75 )

        healthBarPosition =
            ( screen.x + 0.25, screen.y + screen.height - 2 )

        healthBarProgress =
            a.health / a.maxHealth
    in
        [ [ Render.animatedSprite { spellIconSprite | position = spellIconPosition } ]
        , progressBar spellBarPosition spellBarSize spellBarProgress Color.darkBlue
        , progressBar healthBarPosition healthBarSize healthBarProgress Color.red
        ]
            |> List.concat


renderWorld : World -> List Render.Renderable
renderWorld world =
    let
        players =
            world &. (entities3 player transforms rotations)

        enemieEnts =
            world &. (entities3 enemies transforms rotations)

        projectiles =
            world &. (entities playerProjectiles)

        particleEnts =
            world &. (entities particles)
    in
        []
            ++ (drawBackground world.assets world)
            ++ [ drawReticle world ]
            ++ List.map (drawPlayer world.assets) players
            ++ List.map (drawEnemy world.assets) enemieEnts
            ++ List.map (drawProjectile world.assets) projectiles
            ++ List.map (drawParticle world.assets) particleEnts
            ++ (List.map drawHealthBar enemieEnts |> List.concat)
            ++ (List.map (drawUi world) players |> List.concat)
