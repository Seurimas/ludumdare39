module LudumDare39 exposing (..)

{-| -}

import Slime.Engine exposing (..)
import World exposing (..)
import Html exposing (Html, div)
import Game.TwoD as Game
import Game.TwoD.Render as Render
import Game.TwoD.Camera as Camera exposing (Camera)
import Color exposing (Color)


type Msg
    = Noop


subs m =
    Subs.none
        |> engineSubs


engine =
    let
        systems =
            []

        listeners =
            []
    in
        Slime.Engine.initEngine deletor systems listeners


updateWorld =
    Slime.Engine.applySystems engine


takeMessage =
    Slime.Engine.applyListeners engine


update msg model =
    engineUpdate engine msg model


render world =
    Game.render
        { time = 0
        , camera = Camera.fixedHeight world.cameraSize world.cameraPos
        , size = ( 500, 500 )
        }
        ()


main =
    Html.program
        { init = world ! []
        , subscriptions = subs
        , update = update
        , view = render
        }
