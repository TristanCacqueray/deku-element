module DekuWebComponent where

import Prelude

import Effect (Effect)
import Web.DOM.Element as Web.DOM

foreign import defineComponent
  :: forall a. String
  -> Array String
  -> Effect a
  -> (a -> Web.DOM.Element -> Effect Unit)
  -> (a -> String -> String -> Effect Unit)
  -> Effect Unit
