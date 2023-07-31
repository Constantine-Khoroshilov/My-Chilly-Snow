module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Time exposing (Posix, posixToMillis, millisToPosix)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on)

import Json.Decode as Decode
import Random

import Canvas exposing (Point, shapes, group, lineTo)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Text exposing (TextAlign(..), font, align)
import Color

import Tuple exposing (first, second)




-- MODEL


type alias Model =
  { canvSize : (Int, Int)
  , isMobile : Bool
  -- This is the current Posix time (time since 1970)
  -- This is necessary to control the frequency of the call
  -- onAnimationFrame (FPS control), which sends Sub Msg
  -- with the current Posix time.
  , posix : Posix
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
  -- this is the velocity vec all game objects except the ball
  -- it is always negative and affects on Y coord, so directed upwards
  , slipVelocity : Float
  , treesStartPos : List Point
  , treesPos : List Point
  , ball : Ball
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
  -- The velocity vec
  , vx : Float
  -- The acceleration vec 
  , ax : Float
  }

ballVX = 4.5
ballAX = 0.2

ballStartState (canvWidth, canvHeight) =
  { pos = 
      ( toFloat canvWidth |> (*) 0.5
      , toFloat canvHeight |> (*) 0.3
      )
  , radius = toFloat canvWidth |> (*) 0.015
  , vx = ballVX
  , ax = 0
  }

-- max or min by module

minSlipVel = -2
maxSlipVel = -8
slipAcceleration = -0.005




-- The func init gets screen width and height from JS 

init : (Int, Int) -> (Model, Cmd Msg)
init screenSize =
  let
    canvSize = toCanvSize screenSize
    isMobile = (first screenSize) <= (second screenSize)
  in
    loadNextLevel
      { canvSize = canvSize
      , isMobile = isMobile
      , posix = millisToPosix 0
      , clickState = NotHold
      , gameState = Stop
      , level = 0
      , levelSize = second canvSize
      , levelPassed = 0
      , slipVelocity = minSlipVel
      , treesStartPos = []
      , treesPos = []
      , ball = ballStartState canvSize
      }


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
  = Frame Posix
  | SkipFrame
  | SetTreesPos (List Point)
  | ClickDown
  | ClickUp


update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    SkipFrame -> (m, Cmd.none)

    Frame posix ->
      if isCollision m.ball m.treesPos m.canvSize 
      then
        restartLevel m

      else if m.levelPassed >= m.levelSize  
      then
        loadNextLevel m

      else
        ( onFrame m
            |> \model -> { model | posix = posix }
        , Cmd.none
        )


    SetTreesPos pos ->
      ( { m 
          | treesStartPos = pos
          , treesPos = pos 
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
                        | vx = -ball.vx
                        , ax =
                            -- Set the ball acceleration
                            if ball.vx > 0 then -ballAX
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
                        , vx =
                            -- Reset the ball speed
                            if ball.vx > 0 then ballVX
                            else -ballVX
                      } 
        }
      , Cmd.none
      )


onFrame : Model -> Model
onFrame m =
  { m
    | levelPassed = m.levelPassed - (floor m.slipVelocity)
    , slipVelocity = max maxSlipVel (m.slipVelocity + slipAcceleration)
    , ball =
        m.ball
          |> \ball -> 
                { ball 
                  | pos = move ball.vx 0 ball.pos
                  -- Increase speed by acceleration
                  , vx = ball.vx + ball.ax 
                }
    , treesPos =
        List.map (move 0 m.slipVelocity) m.treesPos
  }


move : Float -> Float -> Point -> Point
move dx dy (x, y) =
  (x + dx, y + dy)


isCollision : Ball -> List Point -> (Int, Int) -> Bool
isCollision ball treesPos canvSize =
  let
    (bx, by) = ball.pos
    (w, h) = canvSize
    r = ball.radius
  in
    -- Check collision with the walls
    bx < 0 || bx > (toFloat w) ||
    -- Check collision with the trees
    ( List.filter (\(x, y) -> y < (toFloat h)) treesPos
        |> List.any (\(x, y) -> (bx - x)^2 + (by - y)^2 <= r^2)
    )


loadNextLevel : Model -> (Model, Cmd Msg)
loadNextLevel m =
  let
    -- canvas size
    (w, h) = 
      ( first m.canvSize |> toFloat
      , second m.canvSize |> toFloat
      )

    -- increase the next level size
    levelSize = m.levelSize + 1500
    -- reset ball state
    ball = ballStartState m.canvSize

    l = toFloat levelSize
    ballY = second ball.pos
    -- count of trees
    count = floor (0.015 * l)

    -- paddings
    py = 100
    px = 50

    pointGenerator : Random.Generator Point
    pointGenerator =
      Random.pair
        -- X coord
        (Random.float px (w - px))
        -- Y coord
        (Random.float (ballY + py) (l - ballY - py)) 
  in
    ( { m 
        | gameState = Stop
        , clickState = NotHold
        , level = m.level + 1
        , levelSize = levelSize
        , levelPassed = 0
        , slipVelocity = minSlipVel
        , treesStartPos = []
        , treesPos = []
        , ball = ball
      }
    , Random.list count pointGenerator
        |> Random.generate SetTreesPos
    )


restartLevel m =
  ( { m 
      | gameState = Stop
      , levelPassed = 0
      , slipVelocity = minSlipVel
      , treesPos = m.treesStartPos
      , ball = ballStartState m.canvSize
    }
  , Cmd.none
  )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
  case m.gameState of
    Play ->
      ( \posix -> 
            if (posixToMillis posix) - 
               (posixToMillis m.posix) <= (1000/80 |> floor)
            then
              SkipFrame
            else
              Frame posix
      )
        |> onAnimationFrame

    Stop -> 
      Sub.none
  



-- VIEW


view : Model -> Html Msg
view m =
  Html.main_ []
    [ if checkCanvSize m.canvSize
      then
        Canvas.toHtml m.canvSize
          ( if m.isMobile
            then
              [ on "touchstart" (Decode.succeed ClickDown)
              , on "touchend" (Decode.succeed ClickUp)
              ]
            else
              [ on "mousedown" (Decode.succeed ClickDown)
              , on "mouseup" (Decode.succeed ClickUp)   
              ]
          )
          [ clear m.canvSize
          , finishLine m
          , paintBall m.ball
          , trees m
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


paintBall : Ball -> Canvas.Renderable
paintBall ball =
  shapes 
    [ fill (Color.rgb255 246 74 70) ] 
    [ Canvas.circle ball.pos ball.radius ]


statusBar : Model -> Canvas.Renderable
statusBar m =
  let
    width = m.canvSize |> first |> toFloat
    height = m.canvSize |> second |> toFloat 

    (x1, y1) = (0.3 * width, 0.1 * height)
    (x2, y2) = (0.7 * width, 0.1 * height)
    r = 0.025 * height

    h = r^2 + r^2 |> sqrt
    (x3, y3) = (x1 + h/2, y1 - h/2)
    w1 = x2 - x1 - h

    l = toFloat m.levelSize
    p = toFloat m.levelPassed
    w2 = p / l * w1

    -- general color
    color = Color.rgb255 54 79 107

    print (x, y) fillColor strokeColor level =
      Canvas.text
        [ font { size = 16, family = "Arial" }
        , align Center
        , fill fillColor
        , stroke strokeColor
        ]
        (x, y + 4.5)
        (String.fromInt level)

  in
    group [ stroke color ]
      [ shapes 
          [ fill Color.white
          ]
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

      , print (x1, y1) Color.white Color.white m.level
      , print (x2, y2) color color (m.level + 1)
      ]


finishLine : Model -> Canvas.Renderable
finishLine m =
  let
    width = first m.canvSize |> toFloat
    -- square count
    count = 40
    -- square width (height)
    w = width / count

    posY1 = (m.levelSize - m.levelPassed |> toFloat) + (second m.ball.pos)
    posY2 = posY1 + w

    isEven n =
      (modBy 2 n) == 0

    cell (x, y) color =
      shapes
        [ fill color ]
        [ Canvas.rect (x, y) w w ]

    cellsColors flag = 
      List.range 0 (count - 1)
        |> List.map 
            (\n -> if xor flag (isEven n) then Color.black else Color.white)

    cellLine posY flag =
      List.range 0 (count - 1)
        |> List.map (\n -> ((toFloat n) * w, posY))
        |> List.map2 (\color coords -> cell coords color) (cellsColors flag)
  in
    List.append (cellLine posY1 True) (cellLine posY2 False)
      |> group []


trees : Model -> Canvas.Renderable
trees m =
  let
    width = m.canvSize |> first |> toFloat
    height = m.canvSize |> second |> toFloat

    postH = 0.01 * height
    postW = 0.01 * width

    posts =
      List.map (\(x, y) -> Canvas.rect (x, y - postH) postW postH) m.treesPos
        |> shapes [ fill (Color.rgb255 117 90 87) ]

    bigBase = 0.05 * width

    bigTriangle =
      List.map 
        ( \(x, y) -> 
            Canvas.path ( x - (bigBase / 1.7) + (postW / 2), y - postH       )
              [ lineTo  ( x  + (postW / 2)                 , y - 3.3 * postH )
              , lineTo  ( x + (bigBase / 1.7) + (postW / 2), y - postH       )  
              ]
        ) m.treesPos
          |> shapes [ fill (Color.rgb255 71 167 106), stroke (Color.rgb255 66 94 23) ]

    smallTriangle =
      List.map 
        ( \(x, y) -> 
            Canvas.path ( x - (bigBase / 2) + (postW / 2), y - 2.2 * postH   )
              [ lineTo  ( x + (postW / 2)                , y - 4 * postH     )
              , lineTo  ( x + (bigBase / 2) + (postW / 2), y - 2.2 * postH   )  
              ]
        ) m.treesPos
          |> shapes [ fill (Color.rgb255 71 167 106), stroke (Color.rgb255 66 94 23) ]
  in
    group [] 
      [ posts
      , bigTriangle
      , smallTriangle 
      ]   




-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }