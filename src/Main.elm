module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on)

import Json.Decode as Decode

import Canvas exposing (Point, shapes)
import Canvas.Settings exposing (fill, stroke)
--import Canvas.Settings.Advanced exposing (..)
import Color

import Debug exposing (..)




-- MODEL


type alias Model =
  { canvSize : (Int, Int)
  , clickState : ClickState
  , levelState : LevelState
  -- current game level
  , level : Int
  , ball : Ball
  }


{- This type is needed for changing the moving direction 
   of the ball. 

   If user holds mouse click then the ball will 
   change its direction and get acceleration.

   If user stops holding mouse click 
   the ball will not change its direction 
   but lose its acceleration 
   (its speed will be constantly)
-}

type ClickState
  = Hold | NotHold


type LevelState
  -- Init of the game level (before playing) 
  = Init
  | Pause
  | Play
  -- Game level over. 
  -- True/False is successful/unsuccessful passed level
  -- If is true then new level will be loaded  
  | Over Bool


type alias Ball =
  { pos : Point
  , radius : Float
  -- The speed vec
  , ux : Float
  -- The acceleration vec 
  , ax : Float
  }

ballSpeed = 5 -- without acceleration
ballAcceleration = 0.2




-- The func init gets screen width and height from JS 

init : (Int, Int) -> (Model, Cmd Msg)
init (screenWidth, screenHeight) =
  let
    (canvWidth, canvHeight) = toCanvSize (screenWidth, screenHeight)
  in
    ( { canvSize = 
          ( canvWidth
          , canvHeight
          )
      , clickState = NotHold
      , levelState = Init
      , level = 0
      , ball = 
          { pos = 
              ( toFloat canvWidth |> (*) 0.5
              , toFloat canvHeight |> (*) 0.3
              )
          , radius = toFloat canvWidth |> (*) 0.015
          , ux = ballSpeed
          , ax = 0 
          }
      }

    , Cmd.none
    )


toCanvSize (screenWidth, screenHeight) =
  let
    maxCanvHeight = 915
    maxCanvWidth = 412
  in
    ( min screenWidth maxCanvWidth
    , min screenHeight maxCanvHeight
    )




-- UPDATE


type Msg
  -- It is msg that is called 60 times per sec
  -- for repaint (update) the canvas
  = Frame Float
  | ClickDown
  | ClickUp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Frame _ ->
      case model.levelState of
        Init ->
          ( model
          , Cmd.none
          )

        Pause ->
          ( model
          , Cmd.none
          )

        Play ->
          if isCollision model 
          then
            ( { model | levelState = Over False 
              }
            , Cmd.none 
            )
          else if False 
          then
            ( { model | levelState = Over True 
              }
            , Cmd.none 
            )
          else
            ( onFramePlay model
            , Cmd.none 
            )

        Over isWin ->
          ( model
          , Cmd.none
          )


    ClickDown ->
      ( { model 
          | clickState = Hold
          , levelState = Play
          , ball =
              model.ball
                |> \ball -> 
                      { ball 
                        | ux = -ball.ux
                        , ax =
                            -- Set the ball acceleration
                            if ball.ux > 0 then -ballAcceleration
                            else ballAcceleration
                      }
        }
      , Cmd.none
      )


    ClickUp ->
      ( { model 
          | clickState = NotHold
          , ball = 
              model.ball
                |> \ball -> 
                      { ball 
                        | ax = 0
                        , ux =
                            -- Reset the ball speed
                            if ball.ux > 0 then ballSpeed
                            else -ballSpeed
                      } 
        }
      , Cmd.none
      )


{- The func that updates the game objects (ball, trees...)
   during the game (levelState = Play)
-}

onFramePlay : Model -> Model
onFramePlay model =
  { model 
    | ball =
        model.ball
          |> \ball -> 
                { ball 
                  | pos = move ball.pos ball.ux 0
                  -- Increase speed by acceleration
                  , ux = ball.ux + ball.ax 
                }
  }


move : Point -> Float -> Float -> Point
move (x, y) dx dy =
  (x + dx, y + dy)


isCollision : Model -> Bool
isCollision model =
  let
    (x, y) = model.ball.pos
    (w, h) = model.canvSize
  in
    -- Check collision with the walls
    if x < 0 || x > (toFloat w) then 
      True
    else
      False 




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  onAnimationFrameDelta Frame 
  



-- VIEW


view : Model -> Html Msg
view model =
  Html.main_ []
    [ if checkCanvSize model.canvSize
      then
        Canvas.toHtml model.canvSize
          [ on "touchstart" (Decode.succeed ClickDown)
          , on "touchend" (Decode.succeed ClickUp)
          , on "mousedown" (Decode.succeed ClickDown)
          , on "mouseup" (Decode.succeed ClickUp)   
          ]
          [ clear model.canvSize
          , drawBall model.ball Color.orange
          ]
  
      else
        Html.article 
          [ style "text-align" "center"
          , style "font" "16px Verdana"
          ] 
          [ Html.h1 [] [ text "Упс :(" ]
          , Html.p [] [ text "Слишком маленький размер экрана..." ]
          , Html.p [] [ text "The screen size is too small..." ]
          ]
    ]


checkCanvSize (canvWidth, canvHeight) =
  let
    minCanvHeight = 600
    minCanvWidth = 280
  in
    if canvWidth < minCanvWidth
    || canvHeight < minCanvHeight
    then
      False
    else
      True


clear : (Int, Int) -> Canvas.Renderable
clear (canvWidth, canvHeight) =
  let
    width = toFloat canvWidth
    height = toFloat canvHeight
  in
    Canvas.clear (0, 0) width height


drawBall : Ball -> Color.Color -> Canvas.Renderable
drawBall ball color =
  shapes 
    [ fill color ] 
    [ Canvas.circle ball.pos ball.radius ]




-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }