module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on)

import Json.Decode as Decode

import Canvas exposing (shapes, circle)
import Canvas.Settings exposing (fill, stroke)
--import Canvas.Settings.Advanced exposing (..)
import Color




-- MODEL


type alias Model =
  { canvSize : (Int, Int)
  , clickState : ClickState
  , levelState : LevelState
  -- current game level
  , level : Int
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




-- The func init gets screen width and height from JS 

init : (Int, Int) -> (Model, Cmd Msg)
init (width, height) =
  ( { canvSize = toCanvSize (width, height)
    , clickState = NotHold
    , levelState = Init
    , level = 0
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
      ( model
      , Cmd.none
      )

    ClickDown ->
      ( { model 
          | clickState = Hold
          , levelState = Play
        }
      , Cmd.none
      )

    ClickUp ->
      ( { model | clickState = NotHold }
      , Cmd.none
      )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.levelState of
    Init -> 
      Sub.none

    Pause -> 
      Sub.none

    Play -> 
      onAnimationFrameDelta Frame

    Over _ -> 
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
          [ shapes 
              [ case model.clickState of
                  Hold -> fill Color.red
                  NotHold -> fill Color.green 
              ] 
              [ circle (150, 150) 80 ]
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




-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }