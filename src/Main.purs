module Main where

import Prelude

import Data.Array (head)
import Data.Foldable (for_)
import Data.Int (round)
import Data.Tuple.Nested ((/\))
import Deku.Core (useHot)
import Deku.DOM as D
import Deku.DOM.Self as Self
import Deku.Do as Deku
import Deku.Toplevel (runInBody)
import DekuGrid (grid)
import Effect (Effect)
import Web.ResizeObserver (ResizeObserverBoxOptions(..), observe, resizeObserver)

main :: Effect Unit
main = void $ runInBody $ Deku.do
  _setSize /\ size <- useHot 19
  setDim /\ dimension <- useHot 888
  D.div
    [ Self.self_ \e -> do
        observer <- resizeObserver \entries _oberserver -> do
          for_ (head entries) \entry -> do
            let rect = entry.contentRect
            setDim $ round (min rect.width rect.height)
            pure unit
        observe e { box: BorderBox } observer

    ]
    [ grid { size, dimension }
    ]
