module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Deku.Toplevel (runInBody, runInElement)
import DekuWeiqi (weiqi)
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  mElt <- window >>= document >>= getElementById "weiqi" <<< toNonElementParentNode <<< toDocument
  case mElt of
    Nothing -> log "No weiqi element :/"
    Just elt -> void $ runInElement elt weiqi
