module DekuGrid where

import Prelude

import Data.Array (range)
import Data.Foldable (for_)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (Attribute)
import Deku.Control (text_)
import Deku.Core (Nut, useSkimmed, useState, useState')
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.Self as Self
import Deku.Do as Deku
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FRP.Poll (Poll, animate)
import Graphics.Canvas as Canvas
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element, fromEventTarget, getBoundingClientRect)
import Web.Event.Event (target)
import Web.UIEvent.MouseEvent (MouseEvent, clientY, pageX, toEvent)

type Vertex = { x :: Int, y :: Int }

-- | Runs an effect with the relative mouse coordinate of the target element when it triggers the given event.
coordOn
  :: forall r f
   . Functor f
  => (f (MouseEvent -> Effect Unit) -> f (Attribute r))
  -> f (Vertex -> Effect Unit)
  -> f (Attribute r)
coordOn listener =
  listener <<< map \push ev -> do
    for_ (target (toEvent ev) >>= fromEventTarget) \elt -> do
      rect <- getBoundingClientRect elt
      push { x: pageX ev - round rect.left, y: clientY ev - round rect.top }

coordOn_
  :: forall r f
   . Applicative f
  => (f (MouseEvent -> Effect Unit) -> f (Attribute r))
  -> (Vertex -> Effect Unit)
  -> f (Attribute r)
coordOn_ listener =
  coordOn listener <<< pure

-- | Check if a coordinate can snap to an intersection
snapPos :: forall r. { size :: Int, dim :: Int | r } -> Vertex -> Maybe Vertex
snapPos { size, dim } { x, y } = case (onIntersection && xDist < radius && yDist < radius) of
  true -> Just { x: xCoord, y: yCoord }
  false -> Nothing
  where
  onIntersection = xCoord > 0 && xCoord <= size && yCoord > 0 && yCoord <= size

  cellSize = dim `div` (size + 1)
  cellSize' = toNumber cellSize
  radius = round (cellSize' * 0.4)

  xCoord = round (toNumber xP / cellSize')
  xDist = abs (xP - xCoord * cellSize)
  xP = x - padding

  yCoord = round (toNumber yP / cellSize')
  yDist = abs (yP - yCoord * cellSize)
  yP = y - padding
  padding = (dim - (cellSize * (size + 1))) `div` 2

drawGrid :: { size :: Int, dim :: Int, canvas :: Element } -> Effect Unit
drawGrid { size, dim, canvas } = do
  -- An Element is a Canvas
  let (elt :: Canvas.CanvasElement) = unsafeCoerce canvas
  let dim' = toNumber dim
  Canvas.setCanvasWidth elt dim'
  Canvas.setCanvasHeight elt dim'
  log $ "Drawing: " <> show size <> ", " <> show dim

  ctx <- Canvas.getContext2D elt
  Canvas.withContext ctx do
    Canvas.setFillStyle ctx "#333"

    -- Background
    Canvas.fillPath ctx $ Canvas.rect ctx { x: 0.0, y: 0.0, width: dim', height: dim' }

    let cellSize = dim `div` (size + 1)
    let padding = (dim - (cellSize * (size + 1))) `div` 2

    -- Grid
    Canvas.setStrokeStyle ctx "lightblue"
    Canvas.setLineWidth ctx 1.0
    for_ (range 0 (size - 1)) \n -> do
      Canvas.strokePath ctx $ do
        let
          startPos = toNumber $ padding + cellSize * (n + 1)
          endPos = toNumber $ padding + cellSize * size
        Canvas.moveTo ctx startPos (toNumber $ padding + cellSize)
        Canvas.lineTo ctx startPos endPos
        Canvas.moveTo ctx (toNumber $ padding + cellSize) startPos
        Canvas.lineTo ctx endPos startPos
        Canvas.closePath ctx

    -- Hoshi
    -- TODO: draw star points
    pure unit

grid :: { size :: Poll Int, dimension :: Poll Int, setMouse :: (Maybe Vertex -> Effect Unit) } -> Nut
grid { size, dimension, setMouse } = Deku.do
  -- Store the canvas context
  setCanvas /\ canvas <- useState'

  -- Combine the grid size, dimention and context for rendering purpose
  info <- useSkimmed ({ size: _, dim: _, canvas: _ } <$> size <*> dimension <*> canvas)
  let updateGrid = animate info drawGrid

  -- Store the mouse position for hover animation
  -- setMouse /\ mouse <- useState Nothing

  D.canvas
    [ Self.self_ \elt -> do
        -- Q: is this like component mount event?
        -- Q: do we need to unsubscribe the animation poll?
        void $ updateGrid
        launchAff_ do
          delay (Milliseconds 0.0)
          liftEffect do
            log "Setting canvas"
            setCanvas elt
    , coordOn DL.mousemove $ info <#> \i -> (setMouse <<< snapPos i)
    , DA.style_ "display: grid"
    ]
    [ text_ "Canvas not available, update your browser!" ]
