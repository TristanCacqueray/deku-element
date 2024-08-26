module Main where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Deku.Core (Nut)
import Deku.Toplevel (runInElement)
import DekuWebComponent (defineComponent)
import DekuWeiqi (weiqi, initializeWires, Wires)
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

runInElementId :: String -> Nut -> Effect Unit
runInElementId elementId nut = do
  mElt <- window >>= document >>= getElementById elementId <<< toNonElementParentNode <<< toDocument
  case mElt of
    Nothing -> log "No weiqi element :/"
    Just elt -> void $ runInElement elt nut

setupElement :: Wires -> Element -> Effect Unit
setupElement wires elt = do
  log $ "Setting up element!"
  void $ runInElement elt (weiqi wires)
  log $ "Initialize default value..."
  wires.size.push 19

attrChanged :: Wires -> String -> String -> Effect Unit
attrChanged wires name val = do
  log $ "Attr changed: " <> name <> " = " <> val
  case fromString val of
    Just sz -> wires.size.push sz
    Nothing -> log $ "Invalid size!"

defineWeiqiComponent :: Effect Unit
defineWeiqiComponent =
  defineComponent "deku-weiqi" [ "size" ] initializeWires setupElement attrChanged

main :: Effect Unit
main = do
  wires <- initializeWires
  runInElementId "weiqi" (weiqi wires)
  wires.size.push 19
