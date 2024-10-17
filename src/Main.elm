module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Html exposing (Html, text, main_, article, h1, p)
import Html.Attributes exposing (style)
import Html.Events exposing (on)

import Json.Decode as Decode
import Random

import Canvas exposing (Point, shapes, group, lineTo)
import Canvas.Settings exposing (fill, stroke) 
import Canvas.Settings.Text exposing (TextAlign(..), font, align)
import Color




-- MODEL


type alias Model =
  { canvSize : (Int, Int)
  , clickState : ClickState
  , gameState : GameState
  , time : Float
  , eplasedTime : Float
  , level : Int
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




-- BALL


type alias Ball =
  { x : Float
  , y : Float
  , direction : Int
  , isBoost : Bool
  , radius : Float
  }


updateBallPos ball fps =
  let
    direction = toFloat ball.direction
    decreaser = 60 / fps
    acceleration = 2
    speed = 4.5
  in
    { ball | x = ball.x + direction * decreaser * (speed + 
        if ball.isBoost then acceleration else 0)
    }


updateBall ball isBoost isUpdateDir =
  { ball 
    | direction = (if isUpdateDir then -1 else 1) * ball.direction
    , isBoost = isBoost 
  }


getBall canvSize =
  let
    w = toFloat (Tuple.first canvSize)
    h = toFloat (Tuple.second canvSize)
  in
    { x = 0.5 * w
    , y = 0.3 * h
    , direction = 1
    , isBoost = False
    , radius = 0.015 * w
    }


-- max or min by module

minSlipVel = -2
maxSlipVel = -8
slipAcceleration = -0.005




-- The func init gets screen width and height from JS 

init : (Int, Int) -> (Model, Cmd Msg)
init (sw, sh) =
  let
    canvSize = (min 412 sw, sh)
  in
    loadNextLevel
      { canvSize = canvSize
      , clickState = NotHold
      , gameState = Stop
      , time = 0
      , eplasedTime = 0
      , level = 0
      , ball = getBall canvSize
      }




-- UPDATE


type Msg
  = Frame Float
  | SetTreesPos (List Point)
  | ClickDown
  | ClickUp


update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    Frame delta ->
      ( { m
          | ball = updateBallPos m.ball (1000 / delta)
          , time = m.time + delta
        }
      , Cmd.none
      )


    SetTreesPos pos ->
      ( m
      , Cmd.none
      )


    ClickDown ->
      ( { m 
          | clickState = Hold
          , gameState = Play
          , ball = updateBall m.ball True True
        }
      , Cmd.none
      )


    ClickUp ->
      ( { m 
          | clickState = NotHold
          , ball = updateBall m.ball False False
        }
      , Cmd.none
      )
  

loadNextLevel : Model -> (Model, Cmd Msg)
loadNextLevel m =
  ( { m 
        | gameState = Stop
        , clickState = NotHold
        , level = m.level + 1
        , time = 0
        , eplasedTime = 0
        , ball = getBall m.canvSize
      }
    , Cmd.none
    )


restartLevel m =
  ( { m 
      | gameState = Stop
      , ball = getBall m.canvSize
    }
  , Cmd.none
  )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
  case m.gameState of
    Play ->
      --onAnimationFrame (\posix -> Frame (Time.posixToMillis posix))
      onAnimationFrameDelta Frame

    Stop -> 
      Sub.none
  



-- VIEW


view : Model -> Html Msg
view m =
  main_ []
    [ if checkCanvSize m.canvSize
      then
        Canvas.toHtml m.canvSize mouseEvents
          [ clear m.canvSize
          --, finishLine m
          , paintBall m.ball
          --, trees m
          --, statusBar m
          ]
  
      else
        article 
          [ style "margin-top" "45vh"
          , style "text-align" "center"
          , style "font" "16px Verdana"
          ] 
          [ h1 [] [ text "Упс :(" ]
          , p [] [ text "Слишком маленький размер экрана..." ]
          , p [] [ text "The screen size is too small..." ]
          ]
    ]


checkCanvSize canvSize =
  let
    w = toFloat (Tuple.first canvSize)
    h = toFloat (Tuple.second canvSize)
    minCanvWidth = 280
    minCanvHeight = 600
  in
    w > minCanvWidth && h > minCanvHeight


touchEvents =
  [ on "touchstart" (Decode.succeed ClickDown)
  , on "touchend" (Decode.succeed ClickUp)
  ]


mouseEvents =
  [ on "mousedown" (Decode.succeed ClickDown)
  , on "mouseup" (Decode.succeed ClickUp)   
  ]


clear canvSize =
  let
    w = toFloat (Tuple.first canvSize)
    h = toFloat (Tuple.second canvSize)
  in
    Canvas.clear (0, 0) w h


paintBall ball =
  shapes 
    [ fill (Color.rgb255 246 74 70) ] 
    [ Canvas.circle (ball.x, ball.y) ball.radius ]


--statusBar : Model -> Canvas.Renderable
--statusBar m =
--  let
--    width = m.canvSize |> first |> toFloat
--    height = m.canvSize |> second |> toFloat 

--    (x1, y1) = (0.3 * width, 0.1 * height)
--    (x2, y2) = (0.7 * width, 0.1 * height)
--    r = 0.025 * height

--    h = r^2 + r^2 |> sqrt
--    (x3, y3) = (x1 + h/2, y1 - h/2)
--    w1 = x2 - x1 - h

--    l = toFloat m.levelSize
--    p = toFloat m.levelPassed
--    w2 = p / l * w1

--    -- general color
--    color = Color.rgb255 54 79 107

--    print (x, y) fillColor strokeColor level =
--      Canvas.text
--        [ font { size = 16, family = "Arial" }
--        , align Center
--        , fill fillColor
--        , stroke strokeColor
--        ]
--        (x, y + 4.5)
--        (String.fromInt level)

--  in
--    group [ stroke color ]
--      [ shapes 
--          [ fill Color.white
--          ]
--          [ Canvas.rect (x3, y3) w1 h ]
--      , shapes 
--          [ fill color ]
--          [ Canvas.rect (x3, y3) w2 h ]
--      , shapes 
--          [ fill color ] 
--          [ Canvas.circle (x1, y1) r ]
--      , shapes
--          [ fill Color.white ]
--          [ Canvas.circle (x2, y2) r ]

--      , print (x1, y1) Color.white Color.white m.level
--      , print (x2, y2) color color (m.level + 1)
--      ]


--finishLine : Model -> Canvas.Renderable
--finishLine m =
--  let
--    width = first m.canvSize |> toFloat
--    ballY = second m.ball.pos
--    -- square count
--    count = 40
--    -- square width (height)
--    w = width / count

--    posY1 = (m.levelSize - m.levelPassed |> toFloat) + ballY
--    posY2 = posY1 + w

--    isEven n =
--      (modBy 2 n) == 0

--    cell (x, y) color =
--      shapes
--        [ fill color ]
--        [ Canvas.rect (x, y) w w ]

--    cellsColors flag = 
--      List.range 0 (count - 1)
--        |> List.map 
--            (\n -> if xor flag (isEven n) then Color.black else Color.white)

--    cellLine posY flag =
--      List.range 0 (count - 1)
--        |> List.map (\n -> ((toFloat n) * w, posY))
--        |> List.map2 (\color coords -> cell coords color) (cellsColors flag)
--  in
--    List.append (cellLine posY1 True) (cellLine posY2 False)
--      |> group []


--trees : Model -> Canvas.Renderable
--trees m =
--  let
--    width = m.canvSize |> first |> toFloat
--    height = m.canvSize |> second |> toFloat

--    postH = 0.01 * height
--    postW = 0.01 * width

--    posts =
--      List.map (\(x, y) -> Canvas.rect (x, y - postH) postW postH) m.treesPos
--        |> shapes [ fill (Color.rgb255 117 90 87) ]

--    bigBase = 0.05 * width

--    bigTriangle =
--      List.map 
--        ( \(x, y) -> 
--            Canvas.path ( x - (bigBase / 1.7) + (postW / 2), y - postH       )
--              [ lineTo  ( x  + (postW / 2)                 , y - 3.3 * postH )
--              , lineTo  ( x + (bigBase / 1.7) + (postW / 2), y - postH       )  
--              ]
--        ) m.treesPos
--          |> shapes [ fill (Color.rgb255 71 167 106), stroke (Color.rgb255 66 94 23) ]

--    smallTriangle =
--      List.map 
--        ( \(x, y) -> 
--            Canvas.path ( x - (bigBase / 2) + (postW / 2), y - 2.2 * postH   )
--              [ lineTo  ( x + (postW / 2)                , y - 4 * postH     )
--              , lineTo  ( x + (bigBase / 2) + (postW / 2), y - 2.2 * postH   )  
--              ]
--        ) m.treesPos
--          |> shapes [ fill (Color.rgb255 71 167 106), stroke (Color.rgb255 66 94 23) ]
--  in
--    group [] 
--      [ posts
--      , bigTriangle
--      , smallTriangle 
--      ]   




-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }