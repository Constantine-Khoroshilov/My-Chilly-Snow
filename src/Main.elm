module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on)

import Json.Decode as Decode
import Random

import Canvas exposing (Point, shapes)
import Canvas.Settings exposing (fill, stroke)
--import Canvas.Settings.Advanced exposing (..)
import Color

import Debug exposing (..)




-- MODEL


type alias Model =
  { canvSize : (Int, Int)
  , clickState : ClickState
  , gameState : GameState
  -- the current game level
  , level : Int
  -- the higher the level, the longer the length of the level
  -- measured in pixels
  , levelLength : Int
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

ballStartPos (canvWidth, canvHeight) =
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
init (screenWidth, screenHeight) =
  let
    canvSize = toCanvSize (screenWidth, screenHeight)
    levelLength = 3000 + (Tuple.second canvSize)
  in
    ( { canvSize = canvSize
      , clickState = NotHold
      , gameState = Stop
      , level = 1
      , levelLength = levelLength
      , ball = ballStartPos canvSize
      , trees = Trees [] minTreesUY treesAY
      }
    , initLevel canvSize (ballStartPos canvSize) levelLength
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
update msg model =
  case msg of
    Frame _ ->
      if isCollision model.ball model.trees model.canvSize 
      then
        ( { model | gameState = Stop 
          }
        , initLevel model.canvSize model.ball model.levelLength
        )

      else if isLevelComplete model.trees 
      then
        ( { model 
            | gameState = Stop
            , level = model.level + 1
            , levelLength = model.levelLength + 3000 
          }
        , initLevel model.canvSize model.ball model.levelLength
        )

      else
        ( onFrame model
        , Cmd.none
        )


    LevelInit pos ->
      ( { model
          | clickState = NotHold
          , ball = ballStartPos model.canvSize
          , trees = 
              model.trees 
                |> \trees -> 
                  { trees 
                    | positions = pos
                    , uy = minTreesUY
                  }   
        }
      , Cmd.none
      )


    ClickDown ->
      ( { model 
          | clickState = Hold
          -- Start game
          , gameState = Play
          , ball =
              model.ball
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
      ( { model 
          | clickState = NotHold
          , ball = 
              model.ball
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
onFrame model =
  { model
    | ball =
        model.ball
          |> \ball -> 
                { ball 
                  | pos = move ball.pos ball.ux 0
                  -- Increase speed by acceleration
                  , ux = ball.ux + ball.ax 
                }
    , trees =
        model.trees
          |> \trees ->
                { trees
                  | uy = max maxTreesUY (trees.uy + trees.ay)
                  , positions = 
                      List.map 
                        (\(x, y) -> (x, y + trees.uy)) 
                        trees.positions
                }
  }


move : Point -> Float -> Float -> Point
move (x, y) dx dy =
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
      

isLevelComplete : Trees -> Bool
isLevelComplete trees =
  let
    lastTreeY = 
      List.map Tuple.second trees.positions
        |> List.maximum
        |> Maybe.withDefault 0 
  in
    lastTreeY < 0


initLevel : (Int, Int) -> Ball -> Int -> Cmd Msg
initLevel (width, height) ball levelLength =
  let
    -- paddings
    px = 50
    py = 100

    ballY = Tuple.second ball.pos

    -- canvas size
    (w, h) = (toFloat width, toFloat height)

    -- level length
    l = toFloat levelLength

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
subscriptions model =
  case model.gameState of
    Play ->
      onAnimationFrameDelta Frame

    Stop -> 
      Sub.none
  



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
          , List.map (\pos -> Canvas.circle pos 5) model.trees.positions 
              |> shapes [ fill Color.green ]
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