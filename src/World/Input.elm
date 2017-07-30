module World.Input exposing (..)

import Mouse exposing (Position, moves)
import Keyboard exposing (KeyCode)
import Char
import World.View as View


type Msg
    = MouseDown Position
    | MouseUp Position
    | MouseMove Position
    | KeyUp KeyCode
    | KeyDown KeyCode
    | Noop


type alias MouseState x =
    { x
        | mousePlace : Position
        , mouseDown : Bool
    }


type alias KeyState x =
    { x
        | left : Bool
        , right : Bool
        , down : Bool
        , up : Bool
    }


type alias InputState =
    MouseState (KeyState {})


type alias InteractiveWorld x =
    { x
        | inputState : InputState
    }


initInputState =
    { mousePlace = { x = 0, y = 0 }
    , mouseDown = False
    , left = False
    , right = False
    , up = False
    , down = False
    }


left =
    (Char.toCode 'A')


right =
    (Char.toCode 'D')


up =
    (Char.toCode 'W')


down =
    (Char.toCode 'S')


setKey : KeyCode -> Bool -> InputState -> InputState
setKey keyCode newVal inputState =
    if keyCode == left then
        { inputState | left = newVal }
    else if keyCode == right then
        { inputState | right = newVal }
    else if keyCode == up then
        { inputState | up = newVal }
    else if keyCode == down then
        { inputState | down = newVal }
    else
        inputState


setInputState : InputState -> InteractiveWorld x -> InteractiveWorld x
setInputState inputState world =
    { world | inputState = inputState }


listener : Msg -> InteractiveWorld x -> InteractiveWorld x
listener msg ({ inputState } as world) =
    case msg of
        MouseDown _ ->
            setInputState { inputState | mouseDown = True } world

        MouseUp _ ->
            setInputState { inputState | mouseDown = False } world

        MouseMove pos ->
            setInputState { inputState | mousePlace = pos } world

        KeyUp key ->
            setInputState (setKey key False inputState) world

        KeyDown key ->
            setInputState (setKey key True inputState) world

        _ ->
            world


gameMouse : View.ViewableWorld (InteractiveWorld x) -> ( Float, Float )
gameMouse world =
    let
        { x, y } =
            world.inputState.mousePlace
    in
        View.screenToWorld world ( toFloat x, toFloat y )


subs =
    Sub.batch
        [ moves MouseMove
        , Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
