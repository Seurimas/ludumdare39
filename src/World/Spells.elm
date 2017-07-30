module World.Spells exposing (..)

import Math
import Vector2 exposing (Float2, getX, getY)
import World.Components exposing (..)
import Slime exposing (..)
import World exposing (..)
import Random.Pcg as Pcg
import Whistle.Native
import Task
import Assets.Reference exposing (..)


doRandom : ComponentSpec (Damagable e) World -> { x | id : EntityID } -> List SpellType -> { id : EntityID, damagable : Damagable e } -> World -> World
doRandom targetType spellEnt effects target world =
    let
        pick =
            Pcg.sample effects

        ( picked, nextSeed ) =
            Pcg.step pick world.magicSeed

        do aWorld =
            case picked of
                Just hit ->
                    doEffect targetType spellEnt hit target aWorld

                Nothing ->
                    aWorld
    in
        do { world | magicSeed = nextSeed }


doEffect : ComponentSpec (Damagable e) World -> { x | id : EntityID } -> SpellType -> { id : EntityID, damagable : Damagable e } -> World -> World
doEffect targetType spellEnt hit target =
    case hit of
        Hurt amount ->
            hurtTarget targetType amount target

        Slow amount ->
            slowTarget targetType amount target

        Explode effect size sprite ->
            explode effect size sprite spellEnt.id

        Composite effectA effectB ->
            ((doEffect targetType spellEnt effectA target)) >> ((doEffect targetType spellEnt effectB target))

        Random effects ->
            doRandom targetType spellEnt effects target


spellToEffect : { x | id : EntityID } -> SpellClass -> { id : EntityID, damagable : Enemy } -> ( World -> World, Bool )
spellToEffect spellEnt effect target =
    case effect of
        Proj projEffect ->
            ( doEffect enemies spellEnt projEffect.hit target, not projEffect.penetrate )

        _ ->
            ( identity, False )


explode : SpellType -> Float2 -> Sprite -> EntityID -> (World -> World)
explode effect size sprite source world =
    let
        sources =
            ( getComponentById playerProjectiles source world
            , getComponentById transforms source world
            )

        ( centerX, centerY ) =
            case sources of
                ( Just projectile, _ ) ->
                    projectile.pos

                ( _, Just transform ) ->
                    (Math.center transform)

                _ ->
                    ( 0, 0 )

        burst =
            { x = centerX - (getX size) / 2, y = centerY - (getY size) / 2, width = getX size, height = getY size }

        locations =
            (world &. entities transforms)
                |> List.filter (\ent -> hasComponent enemies ent world || hasComponent player ent world)

        targets =
            locations
                |> List.filter
                    (\ent ->
                        case Math.separation burst ent.a of
                            Just _ ->
                                True

                            Nothing ->
                                False
                    )
                |> List.map
                    (\ent ->
                        ( ent.id, getComponentById enemies ent.id world, getComponentById player ent.id world )
                    )

        hitTarget ( id, enemy, mPlayer ) =
            case ( enemy, mPlayer ) of
                ( Just enemy, _ ) ->
                    doEffect enemies { id = source } effect { id = id, damagable = enemy }

                ( _, Just playerComp ) ->
                    doEffect player { id = source } effect { id = id, damagable = playerComp }

                _ ->
                    identity

        updatedWorld =
            List.foldl hitTarget world targets

        ( _, withParticle ) =
            forNewEntity updatedWorld
                &=> ( particles
                    , { pos = ( centerX, centerY )
                      , vel = ( 0, 0 )
                      , lifeLeft = 0.5
                      , maxLife = 0.5
                      , sprite = sprite
                      , size = size
                      }
                    )
    in
        withParticle


hurtTarget : ComponentSpec (Damagable x) World -> Float -> { id : EntityID, damagable : Damagable x } -> (World -> World)
hurtTarget targets amount target world =
    let
        ( _, updatedWorld ) =
            forEntityById target.id world
                &~> ( targets, Maybe.map (hurt amount) )
    in
        updatedWorld


slowTarget : ComponentSpec (Damagable x) World -> Float -> { id : EntityID, damagable : Damagable e } -> (World -> World)
slowTarget targetType amount target world =
    let
        slow ({ moveSpeed } as enemy) =
            if amount > moveSpeed then
                { enemy | moveSpeed = 0 }
            else
                { enemy | moveSpeed = moveSpeed - amount }

        ( _, updatedWorld ) =
            forEntityById target.id world
                &~> ( targetType, Maybe.map slow )
    in
        updatedWorld


magicMissile : Float -> Spell
magicMissile speedMod =
    { effect =
        Proj
            { hit = Hurt 1
            , projSpeed = 8 * speedMod
            , penetrate = False
            , projectileArt = ProjSprite Magic
            }
    , castSpeed = 0.25
    , castsLeft = 5
    , maxCasts = 5
    , icon = SpellIcon Magic
    , sound = Cast
    }


fireball : Float -> Spell
fireball speedMod =
    { effect =
        Proj
            { hit = Explode (Hurt 2) ( 3, 3 ) (Explosion Firey)
            , projSpeed = 6 * speedMod
            , penetrate = False
            , projectileArt = ProjSprite Fire
            }
    , castSpeed = 0.25
    , castsLeft = 50
    , maxCasts = 50
    , icon = SpellIcon Fire
    , sound = Cast
    }


iceball : Float -> Spell
iceball speedMod =
    { effect =
        Proj
            { hit = Composite (Hurt 1) (Slow 1)
            , projSpeed = 6 * speedMod
            , penetrate = False
            , projectileArt = ProjSprite Ice
            }
    , castSpeed = 0.25
    , castsLeft = 50
    , maxCasts = 50
    , icon = SpellIcon Ice
    , sound = Cast
    }


chromaBall : Float -> Spell
chromaBall speedMod =
    { effect =
        Proj
            { hit =
                Random
                    [ Composite (Hurt 2) (Slow -2)
                    , Explode (Composite (Hurt 1) (Slow 3)) ( 3, 3 ) (Explosion Icey)
                    , Explode (Hurt 2) ( 4, 4 ) (Explosion Firey)
                    , Hurt 0.5
                    , Hurt 1
                    ]
            , projSpeed = 6 * speedMod
            , penetrate = False
            , projectileArt = ProjSprite Chroma
            }
    , castSpeed = 1
    , castsLeft = 50
    , maxCasts = 50
    , icon = SpellIcon Chroma
    , sound = Cast
    }


spellGen : Pcg.Generator Spell
spellGen =
    let
        randomizeProjSpeed base =
            Pcg.float 0.85 1.15
                |> Pcg.map
                    (\speedMod ->
                        base speedMod
                    )

        randomizeCastSpeed base =
            Pcg.float 0.15 0.3
                |> Pcg.map
                    (\castSpeed ->
                        { base
                            | castSpeed = castSpeed
                        }
                    )

        randomizeCasts base =
            Pcg.int 20 50
                |> Pcg.map
                    (\casts ->
                        { base
                            | maxCasts = casts
                            , castsLeft = casts
                        }
                    )

        randomize base =
            randomizeProjSpeed base
                |> Pcg.andThen randomizeCasts
                |> Pcg.andThen randomizeCastSpeed

        generators =
            [ randomize iceball
            , randomize fireball
            , randomize magicMissile
            , randomize chromaBall
            ]
    in
        Pcg.choices generators


newSpell : Entity Player -> Pcg.Seed -> ( Entity Player, Pcg.Seed )
newSpell ({ a } as ent) seed =
    let
        ( randomSpell, newSeed ) =
            Pcg.step spellGen seed
    in
        ( { ent | a = { a | spell = randomSpell } }, newSeed )


switchSpell : World -> World
switchSpell world =
    let
        depleteAndReplace ( { a } as ent, seed ) =
            if a.spell.castsLeft <= 0 then
                if not world.inputState.mouseDown then
                    newSpell ent seed
                else
                    ( { ent | a = { a | spell = fizzle } }, seed )
            else
                ( ent, seed )

        ( updatedWorld, updatedSeed ) =
            stepEntitiesWith (entities player) depleteAndReplace ( world, world.magicSeed )
    in
        { updatedWorld | magicSeed = updatedSeed }
