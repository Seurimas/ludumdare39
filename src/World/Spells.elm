module World.Spells exposing (..)

import World.Components exposing (..)
import Slime exposing (..)
import World exposing (..)
import Random.Pcg as Pcg
import Whistle.Native
import Task
import Assets.Reference exposing (..)


spellToEffect : { x | id : EntityID } -> SpellClass -> { id : EntityID, damagable : Enemy } -> ( World -> World, Bool )
spellToEffect spellEnt effect target =
    let
        doEffect hit =
            case hit of
                Hurt amount ->
                    hurtTarget amount target

                Slow amount ->
                    slowTarget amount target

                Composite effectA effectB ->
                    ((doEffect effectA)) >> ((doEffect effectB))
    in
        case effect of
            Proj projEffect ->
                ( doEffect projEffect.hit, not projEffect.penetrate )

            _ ->
                ( identity, False )


hurtTarget : Float -> { id : EntityID, damagable : Enemy } -> (World -> World)
hurtTarget amount target world =
    let
        ( _, updatedWorld ) =
            forEntityById target.id world
                &~> ( enemies, Maybe.map (hurt amount) )
    in
        updatedWorld


slowTarget : Float -> { id : EntityID, damagable : Enemy } -> (World -> World)
slowTarget amount target world =
    let
        slow ({ moveSpeed } as enemy) =
            if amount > moveSpeed then
                { enemy | moveSpeed = 0 }
            else
                { enemy | moveSpeed = moveSpeed - amount }

        ( _, updatedWorld ) =
            forEntityById target.id world
                &~> ( enemies, Maybe.map slow )
    in
        updatedWorld


magicMissile : Float -> Spell
magicMissile speedMod =
    { effect =
        Proj
            { hit = Hurt 1
            , projSpeed = 8 * speedMod
            , penetrate = False
            , projectileArt = ProjMagic
            }
    , castSpeed = 0.25
    , castsLeft = 5
    , maxCasts = 5
    , icon = SpellMagic
    , sound = Fire
    }


fireball : Float -> Spell
fireball speedMod =
    { effect =
        Proj
            { hit = Hurt 2
            , projSpeed = 6 * speedMod
            , penetrate = False
            , projectileArt = ProjFire
            }
    , castSpeed = 0.25
    , castsLeft = 50
    , maxCasts = 50
    , icon = SpellFire
    , sound = Fire
    }


iceball : Float -> Spell
iceball speedMod =
    { effect =
        Proj
            { hit = Composite (Hurt 1) (Slow 0.4)
            , projSpeed = 6 * speedMod
            , penetrate = False
            , projectileArt = ProjIce
            }
    , castSpeed = 0.25
    , castsLeft = 50
    , maxCasts = 50
    , icon = SpellIce
    , sound = Fire
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
