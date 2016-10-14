module Main exposing (..)

import Html exposing (div, text, Html)
import Html.Attributes exposing (style, class)
import Html.App as App
import Keyboard
import Time exposing (Time, every, millisecond)
import Array


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { character : Character
    , keyDown : Bool
    , arrow : Arrow
    , map : Map
    }


type alias Map =
    { width : Int
    , height : Int
    }


type alias Character =
    { location : Point
    , spriteFrame : Int
    , spriteLocation : List Point
    }


type alias Point =
    { x : Int
    , y : Int
    }


type Msg
    = KeyDown Arrow
    | KeyUp Arrow
    | Tick Time


type Arrow
    = Up
    | Down
    | Left
    | Right
    | Other


init : ( Model, Cmd Msg )
init =
    { character = characterInit, keyDown = False, arrow = Other, map = Map 1400 1040 } ! []


characterInit : Character
characterInit =
    { location = Point 150 -50, spriteFrame = (List.length spriteLocation - 1), spriteLocation = spriteLocation }


spriteLocation : List Point
spriteLocation =
    [ Point -5 -6, Point -298 -4, Point -593 -4, Point -890 -6, Point -1188 -7, Point -1480 -5, Point -1773 -3, Point -2068 -3, Point -2365 -5, Point -2663 -7 ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown arr ->
            { model | keyDown = True, arrow = arr } ! []

        KeyUp arr ->
            let
                char =
                    model.character
            in
                { model | keyDown = False, arrow = arr, character = { char | spriteFrame = (List.length spriteLocation) - 1 } } ! []

        Tick time ->
            { model | character = characterAnimate model } ! []


moveCharacter : Model -> Character
moveCharacter model =
    let
        char =
            model.character

        map =
            model.map
    in
        case model.arrow of
            Up ->
                model.character

            Down ->
                model.character

            Left ->
                let
                    newPoint =
                        if model.character.location.x <= 100 then
                            1450
                        else
                            char.location.x - 5
                in
                    { char | location = Point newPoint char.location.y }

            Right ->
                let
                    newPoint =
                        if model.character.location.x >= map.width then
                            100
                        else
                            char.location.x + 5
                in
                    { char | location = Point newPoint char.location.y }

            Other ->
                model.character


updateSpriteLocation : Character -> Character
updateSpriteLocation char =
    { char | spriteFrame = List.length spriteLocation |> incrementSpriteFrame char.spriteFrame }


characterAnimate : Model -> Character
characterAnimate =
    moveCharacter >> updateSpriteLocation


incrementSpriteFrame : Int -> Int -> Int
incrementSpriteFrame index arrLen =
    if index + 1 >= arrLen then
        0
    else
        index + 1


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.keyDown of
        False ->
            Sub.batch
                [ Keyboard.downs keyDownHandlerMsg
                , Keyboard.ups keyUpHandlerMsg
                ]

        True ->
            Sub.batch
                [ Keyboard.downs keyDownHandlerMsg
                , Keyboard.ups keyUpHandlerMsg
                , every (75 * millisecond) Tick
                ]


view : Model -> Html Msg
view model =
    let
        char =
            model.character

        locString =
            pointToString char.location
    in
        div
            [ class "character", style [ ( "background", imgHelper char.spriteLocation char.spriteFrame ), ( "height", "435px" ), ( "width", "287px" ), ( "left", locString.x ), ( "top", locString.y ), ( "position", "absolute" ) ] ]
            []


pointToString : Point -> { x : String, y : String }
pointToString point =
    { x = toString point.x ++ "px"
    , y = toString point.y ++ "px"
    }


imgHelper : List Point -> Int -> String
imgHelper spritelist index =
    let
        item =
            Array.fromList spritelist
                |> Array.get index
                |> Maybe.withDefault { x = 0, y = 0 }

        pointStr =
            (toString item.x) ++ "px " ++ (toString item.y) ++ "px"
    in
        "url('assets/sprites/walking.png') no-repeat " ++ pointStr


keyDownHandlerMsg : Keyboard.KeyCode -> Msg
keyDownHandlerMsg =
    keyHandler >> KeyDown


keyUpHandlerMsg : Keyboard.KeyCode -> Msg
keyUpHandlerMsg =
    keyHandler >> KeyUp


keyHandler : Keyboard.KeyCode -> Arrow
keyHandler key =
    case key of
        38 ->
            Up

        40 ->
            Down

        37 ->
            Left

        39 ->
            Right

        _ ->
            Other
