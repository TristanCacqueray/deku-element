module DekuGrid where

import Prelude

import Data.Foldable (for_)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, text, useHot, useState)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.Self as Self
import Deku.Do as Deku
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.Element (getBoundingClientRect)

import Web.UIEvent.MouseEvent (MouseEvent, clientY, pageX)

mouseOn
  :: forall r f
   . Functor f
  => (f (MouseEvent -> Effect Unit) -> f (Attribute r))
  -> f (MouseEvent -> Effect Unit)
  -> f (Attribute r)
mouseOn listener =
  listener <<< map identity

-- import Web.ResizeObserver (ResizeObserverBoxOptions(..), observe, resizeObserver)
grid :: Nut
grid = Deku.do
  setCanvas /\ canvas <- useHot Nothing
  setMouse /\ mouse <- useState { x: 0, y: 0 }
  D.div [ DA.style_ "display: grid" ]
    [ D.span__ "deku-grid"
    , D.canvas
        [ -- Detect resize event, unfortunately that doesn't give the initial canvas position
          -- Self.self_ \e -> do
          --   observer <- resizeObserver \entries _oberserver ->
          --     case head entries of
          --       Just entry -> logShow entry.contentRect
          --       Nothing -> pure unit
          --   observe e { box: BorderBox } observer
          Self.self_ \s -> do
            -- Store the canvas ref so that the mousemove can read it's bouding rect
            setCanvas $ Just s
        , DA.width_ "200"
        , DA.height_ "200"
        , DA.style_ "border-color:pink; border-style:dashed;"
        , mouseOn DL.mousemove $ canvas <#> \mElt -> do
            \event -> do
              for_ mElt \elt -> do
                -- the canvas is set, get its offset
                rect <- getBoundingClientRect elt
                setMouse { x: pageX event - floor rect.left, y: clientY event - floor rect.top }

        ]
        []
    , text $ mouse <#> \m -> ("Mouse pos: " <> show m)
    ]
