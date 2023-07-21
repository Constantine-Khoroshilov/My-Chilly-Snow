module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on)

import Json.Decode as Decode
import Random

import Canvas exposing (Point, shapes, group)
import Canvas.Settings exposing (fill, stroke)
--import Canvas.Settings.Advanced exposing (..)
import Color

import Tuple exposing (first, second)

import Debug exposing (..)




-- MODEL


type alias Model =
  { canvSize : (Int, Int)
  , clickState : ClickState
  , gameState : GameState
  -- the current game level
  , level : Int
  -- the higher the level num, the longer the size of the level
  -- measured in pixels by height 
  , levelSize : Int
  -- the passed part of a level during the game
  -- measured in pixels by height
  , levelPassed : Int
  , ball : Ball
  , trees : Trees
  }


{- This type (ClickState) is needed for changing the 
   moving direction of the ball. 

   If user holds mouse click then the ball will 
   change its direction and get acceleration.

   If user stops holding mouse click 
   the ball will not change its direction 
   but lose its acceleration 
   (its speed will be constantly)
-}

type ClickState
  = Hold | NotHold


type GameState
  = Play | Stop


-- The game entities

type alias Ball =
  { pos : Point
  , radius : Float
  -- The speed vec
  , ux : Float
  -- The acceleration vec 
  , ax : Float
  }

ballUX = 5
ballAX = 0.2

ballStartState (canvWidth, canvHeight) =
  { pos = 
      ( toFloat canvWidth |> (*) 0.5
      , toFloat canvHeight |> (*) 0.3
      )
  , radius = toFloat canvWidth |> (*) 0.015
  , ux = ballUX
  , ax = 0
  }


type alias Trees = 
    { positions : List Point
    -- uy and ay vecs must be less 0 
    , uy : Float
    , ay : Float
    }

-- max or min by module

minTreesUY = -2
maxTreesUY = -10
treesAY = -0.015




-- The func init gets screen width and height from JS 

init : (Int, Int) -> (Model, Cmd Msg)
init screenSize =
  let
    canvSize = toCanvSize screenSize
    levelSize = 3000 + (second canvSize)
    ball = ballStartState canvSize
  in
    ( { canvSize = canvSize
      , clickState = NotHold
      , gameState = Stop
      , level = 1
      , levelSize = levelSize
      , levelPassed = 0
      , ball = ball
      , trees =
          { positions = []
          , uy = minTreesUY
          , ay = treesAY
          }
      }
    , initLevel canvSize ball levelSize
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
  | LevelInit (List Point)
  | ClickDown
  | ClickUp


update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    Frame _ ->
      if isCollision m.ball m.trees m.canvSize 
      then
        ( { m | gameState = Stop 
          }
        , initLevel m.canvSize m.ball m.levelSize
        )

      else if m.levelPassed >= m.levelSize  
      then
        ( { m 
            | gameState = Stop
            , level = m.level + 1
            , levelSize = m.levelSize + 3000 
          }
        , initLevel m.canvSize m.ball m.levelSize
        )

      else
        ( onFrame m
        , Cmd.none
        )


    LevelInit pos ->
      ( { m
          | clickState = NotHold
          , levelPassed = 0
          , ball = ballStartState m.canvSize
          , trees = 
              m.trees 
                |> \trees -> 
                      { trees 
                        | positions = pos
                        , uy = minTreesUY
                      }   
        }
      , Cmd.none
      )


    ClickDown ->
      ( { m 
          | clickState = Hold
          -- Start game
          , gameState = Play
          , ball =
              m.ball
                |> \ball -> 
                      { ball 
                        | ux = -ball.ux
                        , ax =
                            -- Set the ball acceleration
                            if ball.ux > 0 then -ballAX
                            else ballAX
                      }
        }
      , Cmd.none
      )


    ClickUp ->
      ( { m 
          | clickState = NotHold
          , ball = 
              m.ball
                |> \ball -> 
                      { ball 
                        | ax = 0
                        , ux =
                            -- Reset the ball speed
                            if ball.ux > 0 then ballUX
                            else -ballUX
                      } 
        }
      , Cmd.none
      )


onFrame : Model -> Model
onFrame m =
  { m
    | levelPassed = m.levelPassed - (floor m.trees.uy)
    , ball =
        m.ball
          |> \ball -> 
                { ball 
                  | pos = move ball.ux 0 ball.pos
                  -- Increase speed by acceleration
                  , ux = ball.ux + ball.ax 
                }
    , trees =
        m.trees
          |> \trees ->
                { trees
                  | uy = max maxTreesUY (trees.uy + trees.ay)
                  , positions = 
                      trees.positions
                        |> List.map (move 0 trees.uy)
                        
                }
  }


move : Float -> Float -> Point -> Point
move dx dy (x, y) =
  (x + dx, y + dy)


isCollision : Ball -> Trees -> (Int, Int) -> Bool
isCollision ball trees canvSize =
  let
    (bx, by) = ball.pos
    (w, h) = canvSize
    r = ball.radius
  in
    -- Check collision with the walls
    bx < 0 || bx > (toFloat w) ||
    -- Check collision with the trees
    ( List.filter (\(x, y) -> y < (toFloat h)) trees.positions
        |> List.any (\(x, y) -> (bx - x)^2 + (by - y)^2 <= r^2)
    )


initLevel : (Int, Int) -> Ball -> Int -> Cmd Msg
initLevel (width, height) ball levelSize =
  let
    -- paddings
    px = 50
    py = 100

    ballY = second ball.pos

    -- canvas size
    (w, h) = (toFloat width, toFloat height)

    l = toFloat levelSize

    pointGenerator : Random.Generator Point
    pointGenerator =
      Random.pair
        (Random.float px (w - px))
        (Random.float (ballY + py) (l - py)) 

    count = floor (0.015 * l)
  in
    Random.list count pointGenerator
      |> Random.generate LevelInit



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
  case m.gameState of
    Play ->
      onAnimationFrameDelta Frame

    Stop -> 
      Sub.none
  



-- VIEW


view : Model -> Html Msg
view m =
  Html.main_ []
    [ if checkCanvSize m.canvSize
      then
        Canvas.toHtml m.canvSize
          [ on "touchstart" (Decode.succeed ClickDown)
          , on "touchend" (Decode.succeed ClickUp)
          , on "mousedown" (Decode.succeed ClickDown)
          , on "mouseup" (Decode.succeed ClickUp)   
          ]
          [ clear m.canvSize
          , List.map (\pos -> Canvas.circle pos 5) m.trees.positions 
              |> shapes [ fill Color.green ]
          , drawBall m.ball Color.orange
          , statusBar m
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


statusBar : Model -> Canvas.Renderable
statusBar m =
  let
    width = m.canvSize |> first |> toFloat
    height = m.canvSize |> second |> toFloat 

    top = 0.1 * height

    (x1, y1) = (0.3 * width, top)
    (x2, y2) = (0.7 * width, top)
    r = 0.025 * height

    h = r^2 + r^2 |> sqrt
    (x3, y3) = (x1 + h/2, top - h/2)
    w1 = x2 - x1 - h

    l = toFloat m.levelSize
    p = toFloat m.levelPassed

    w2 = p / l * w1

    color = Color.rgb255 54 79 107
  in
    group [ stroke color ]
      [ shapes
          [ fill Color.white ]
          [ Canvas.rect (x3, y3) w1 h ]
      , shapes
          [ fill color ]
          [ Canvas.rect (x3, y3) w2 h ]
      , shapes 
          [ fill color ] 
          [ Canvas.circle (x1, y1) r ]
      , shapes
          [ fill Color.white ]
          [ Canvas.circle (x2, y2) r ]
      ] 




-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }