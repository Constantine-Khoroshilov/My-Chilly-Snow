module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Html exposing (Html, text, main_, article, h1, p)
import Html.Attributes exposing (style)
import Html.Events exposing (on)

import Json.Decode as Decode
import Random exposing (Generator)

import Canvas exposing (Point, shapes, group, lineTo)
import Canvas.Settings exposing (fill, stroke) 
import Canvas.Settings.Text exposing (TextAlign(..), font, align)
import Color exposing (Color, black, white)




-- MODEL


type alias Model =
  { canvSize : (Int, Int)
  , clickState : ClickState
  , gameState : GameState
  , totalTime : Float -- ms
  , eplasedTime : Float -- ms
  , level : Int
  , ball : Ball
  , finishLine : Movable 
  , trees : List Movable

  -- the queue of trees that are rendered on the canvas

  , renderTQueue : List Movable 

  -- the queue of trees waiting to be rendered on the canvas

  , noRenderTQueue : List Movable
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




-- MOVABLE


{- The movable is an object that moves only along the y axis,
   its y coordinate is calculated using the formula of motion 
   with constant acceleration. 
-}

type alias Movable =
  { x : Float
  , y : Float

  -- the distance is the initial value of 
  -- the y coordinate of the movable object

  , distance : Float
  }


setDistance offset time movable =
  let d = calcDistance offset time in
    { movable | distance = d, y = d }


updateMovable time movable =
  { movable | y = calcPos movable.distance time }


-- The speed and the acceleration of the movable objects

ms = 25
ma = 5


calcDistance offset time =
  let t = 0.001 * time in
    offset + t * (ms + 0.5 * t * ma)


{- The following function is necessary to calculate 
   the y coordinate of the movable objects at a certain time moment.

   The distance is the initial value of the y coordinate.
-}

calcPos distance timeMoment =
  let t = 0.001 * timeMoment in
    distance - t * (ms + 0.5 * t * ma)




-- BALL


type alias Ball =
  { x : Float
  , y : Float
  , direction : Int
  , isBoost : Bool
  , radius : Float

  -- The decreaser is a value that is necessary to ensure 
  -- that the movement of the ball is constant on devices 
  -- with different frame rates per second.

  , decreaser : Float
  }


updateBallPos ball fps =
  let
    direction = toFloat ball.direction
    acceleration = 1.5
    speed = 3.0
  in
    { ball | x = ball.x + ball.decreaser * direction * (speed + 
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
    , decreaser = 1
    }


setDecreaser ball delta =
  let fps = 1000 / delta in 
    -- The decreaser cannot be more than 1 and less than 0.5
    { ball | decreaser = clamp 0.5 1 (60 / fps) }


isDecreaserUnset ball =
  ball.decreaser == 1




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

      -- totalTime is the duraction of a level,
      -- the duraction of the first level is equal 15 seconds
      -- because for each new level its duraction is increased by 10 seconds.

      , totalTime = 5000 -- ms
      , eplasedTime = 0
      , level = 0
      , ball = getBall canvSize
      , finishLine = Movable 0 (toFloat sh) 0
      , trees = []
      , renderTQueue = []
      , noRenderTQueue = []
      }




-- UPDATE


type Msg
  = Frame Float
  | GotFrameDelta Float
  | GotTrees (List Movable)
  | ClickDown
  | ClickUp


update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    Frame delta ->
      if isLevelPassed m then
        loadNextLevel m

      else if isCollision m then
        restartLevel m

      else 
        let 
          t = m.eplasedTime + delta

          renderQueues = 
            Tuple.second m.canvSize
              |> updateRenderTQ m.renderTQueue m.noRenderTQueue t 

          -- Removing the movable objects that have left the canvas 

          renderTQ =
            Tuple.first renderQueues
              |> List.filter (\movable -> movable.y > 0)

          noRenderTQ =
            Tuple.second renderQueues
        in
          ( { m
              | eplasedTime = t
              , ball = updateBallPos m.ball (1000 / delta)
              , finishLine = updateMovable t m.finishLine
              , renderTQueue = List.map (updateMovable t) renderTQ
              , noRenderTQueue = noRenderTQ
            }
          , Cmd.none
          )


    GotFrameDelta delta ->
      ( { m | ball = (setDecreaser m.ball delta) }, Cmd.none )


    GotTrees trees ->
      let
        sortedTrees = 
          List.sortBy (\tree -> tree.distance) trees

        -- Splitting the sorted list of trees into 
        -- a rendering queue and no rendering queue.

        (renderTQ, noRenderTQ) =
          Tuple.second m.canvSize
            |> updateRenderTQ [] sortedTrees m.totalTime
      in
      ( { m 
          | trees = sortedTrees
          , renderTQueue = renderTQ
          , noRenderTQueue = noRenderTQ
          }
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


isLevelPassed m =
  m.totalTime <= m.eplasedTime


isCollision m =
  let
    cw = toFloat (Tuple.first m.canvSize)
    ch = toFloat (Tuple.second m.canvSize)
    bx = m.ball.x
    by = m.ball.y
  in
    bx >= cw || bx <= 0
  

loadNextLevel : Model -> (Model, Cmd Msg)
loadNextLevel m =
  let t = m.totalTime + 10000 in
    ( { m 
        | gameState = Stop
        , clickState = NotHold
        , level = m.level + 1 
        , totalTime = t
        , eplasedTime = 0
        , ball = getBall m.canvSize
        , finishLine = setDistance m.ball.y t m.finishLine
      }
    , Random.generate GotTrees
        <| treesGenerator t m.ball.y 
        <| toFloat (Tuple.first m.canvSize) 
    )


restartLevel :  Model -> (Model, Cmd Msg)
restartLevel m =
  let
    (renderTQ, noRenderTQ) =
      Tuple.second m.canvSize
        |> updateRenderTQ [] m.trees m.totalTime
  in
  ( { m 
      | gameState = Stop
      , eplasedTime = 0
      , ball = getBall m.canvSize
      , finishLine = setDistance m.ball.y m.totalTime m.finishLine
      , renderTQueue = renderTQ
      , noRenderTQueue = noRenderTQ
    }
  , Cmd.none
  )


treesGenerator : Float -> Float -> Float -> Generator (List Movable)
treesGenerator totalTime ballY canvWidth =
  let
    paddingY = 50
    paddingX = 50

    minDistance = ballY + paddingY
    maxDistance = calcDistance ballY totalTime - paddingY 

    count = round (0.02 * maxDistance)
  in
    Random.float paddingX (canvWidth - paddingX)
      |> Random.map2 (\y x -> Movable x y y) (Random.float minDistance maxDistance)
      |> Random.list count


{- The following function moves the movable objects 
   from the noRenderTQueue to the renderTQueue 
   if it is time to process and display them.
-}

updateRenderTQ renderTQ noRenderTQ timeMoment canvHeight =
  case noRenderTQ of
    head :: tail ->
      let
        height = toFloat canvHeight
        position = calcPos head.distance timeMoment
      in
        if position < height then
          updateRenderTQ (head :: renderTQ) tail timeMoment canvHeight

        else
          (renderTQ, noRenderTQ) 

    [] ->
      (renderTQ, noRenderTQ)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
  case m.gameState of
    Play ->
      onAnimationFrameDelta Frame

    Stop -> 
      if isDecreaserUnset m.ball then
        onAnimationFrameDelta GotFrameDelta 
      else
        Sub.none
  



-- VIEW


view : Model -> Html Msg
view m =
  main_ []
    [ if checkCanvSize m.canvSize
      then
        Canvas.toHtml m.canvSize mouseEvents
          [ clear m.canvSize
          , finishLine m
          , paintBall m.ball
          , paintTrees m
          , statusBar m
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


statusBar : Model -> Canvas.Renderable
statusBar m =
  let
    (w, h) = Tuple.mapBoth toFloat toFloat m.canvSize

    -- the circles coords 
    (x1, y1) = (0.3 * w, 0.1 * h)
    (x2, y2) = (0.7 * w, 0.1 * h)

    r = 0.020 * h

    -- the height of the rectangle is calculated 
    -- using the Pythagorean theorem
    rectHeight = sqrt (r * r + r * r)

    -- the rectangles coords
    -- x1 is calculated using the median property in a right triangle
    (x3, y3) = (x1 + 0.5 * rectHeight, y1 - 0.5 * rectHeight)

    -- the width of the 1st rectangle
    rect1Width = x2 - x1 - rectHeight

    -- the width of the 2nd rectangle
    rect2Width = (m.eplasedTime / m.totalTime) * rect1Width

    grey = Color.rgb255 54 79 107

    displayLevel (x, y) color lvl =
      let
        l = String.fromInt lvl
        settings =
          [ font { size = round r, family = "Arial" }
          , align Center
          , stroke color
          , fill color
          ]
      in
        Canvas.text settings (x, y + 0.35 * r) l
  in
    group [ stroke grey ]
      [ shapes 
          [ fill Color.white ]
          [ Canvas.rect (x3, y3) rect1Width rectHeight ]
      , shapes 
          [ fill grey ]
          [ Canvas.rect (x3, y3) rect2Width rectHeight ]
      , shapes 
          [ fill grey ] 
          [ Canvas.circle (x1, y1) r ]
      , shapes
          [ fill white ]
          [ Canvas.circle (x2, y2) r ]

      , displayLevel (x1, y1) white m.level
      , displayLevel (x2, y2) grey (m.level + 1)
      ]


finishLine : Model -> Canvas.Renderable
finishLine m =
  let
    cw = toFloat (Tuple.first m.canvSize)

    countSquares = 80
    squareSize = cw / countSquares * 2
    
    getColor n c1 c2 = 
      if modBy 2 n == 0 then c1 else c2

    square (x, y) color =
      shapes
        [ fill color ]
        [ Canvas.rect (x, y) squareSize squareSize ]

    squares (x, y) n =
      if n > 1 then
        let
          curSquaresPair =  
            [ square (x, y) (getColor n black white)
            , square (x, y + squareSize) (getColor n white black) 
            ]
          nextSquaresPair = squares (x + squareSize, y) (n - 1)
        in
          curSquaresPair ++ nextSquaresPair
      else 
        []
      
    line { x, y, distance } =
      squares (x, y) countSquares |> group []
  in
    line m.finishLine


paintTrees : Model -> Canvas.Renderable
paintTrees m =
  let
    width = m.canvSize |> Tuple.first |> toFloat
    height = m.canvSize |> Tuple.second |> toFloat

    postH = 0.01 * height
    postW = 0.01 * width

    posts =
      List.map (\{ x, y, distance } -> Canvas.rect (x, y - postH) postW postH) m.renderTQueue 
        |> shapes [ fill (Color.rgb255 117 90 87) ]

    bigBase = 0.05 * width

    bigTriangle =
      List.map 
        ( \{ x, y, distance } -> 
            Canvas.path ( x - (bigBase / 1.7) + (postW / 2), y - postH       )
              [ lineTo  ( x  + (postW / 2)                 , y - 3.3 * postH )
              , lineTo  ( x + (bigBase / 1.7) + (postW / 2), y - postH       )  
              ]
        ) m.renderTQueue 
          |> shapes [ fill (Color.rgb255 71 167 106), stroke (Color.rgb255 66 94 23) ]

    smallTriangle =
      List.map 
        ( \{ x, y, distance } -> 
            Canvas.path ( x - (bigBase / 2) + (postW / 2), y - 2.2 * postH   )
              [ lineTo  ( x + (postW / 2)                , y - 4 * postH     )
              , lineTo  ( x + (bigBase / 2) + (postW / 2), y - 2.2 * postH   )  
              ]
        ) m.renderTQueue 
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