module DekuWeiqi where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, useHot, useState)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Self as Self
import Deku.Do as Deku
import DekuGrid (grid)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FRP.Event (EventIO, filterMap, mapAccum)
import FRP.Event as E
import FRP.Poll (Poll, sham)
import Web.ResizeObserver (ResizeObserverBoxOptions(..), observe, resizeObserver)

-- | Remove events that repeat the last value
discardRepeated :: forall a. Eq a => Poll a -> Poll a
discardRepeated = filterMap isDiff <<< mapAccum keepPrev Nothing
  where
  isDiff :: Tuple (Maybe a) a -> Maybe a
  isDiff (Nothing /\ new) = Just new
  isDiff (Just prev /\ new)
    | prev /= new = Just new
    | otherwise = Nothing
  keepPrev prev new = Tuple (Just new) (Tuple prev new)

type Wires =
  { size :: EventIO Int
  }

initializeWires :: Effect Wires
initializeWires = do
  size <- liftST $ E.create
  pure { size }

weiqi :: Wires -> Nut
weiqi wires = Deku.do
  let size = sham wires.size.event
  -- _setSize /\ size <- useHot 9
  setDim /\ dimension <- useHot 888

  -- Store the mouse position for hover animation
  setMouse /\ mouse <- useState Nothing

  D.div
    [ Self.self_ \e -> do
        launchAff_ do
          delay (Milliseconds 0.0)
          liftEffect do
            observer <- resizeObserver \entries _oberserver -> do
              for_ (head entries) \entry -> do
                let rect = entry.contentRect
                setDim $ floor (min rect.width rect.height)
                pure unit
            observe e { box: BorderBox } observer
    , DA.style_ "height: 100%; margin: 0; padding: 0; width: 100%; "
    ]
    [ grid { size: discardRepeated size, dimension: discardRepeated dimension, setMouse }
    -- , text $ mouse <#> \m -> ("Mouse pos: " <> show m)
    ]
