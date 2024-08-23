module Main where

import Prelude

import Deku.Toplevel (runInBody)
import Effect (Effect)

import DekuGrid (grid)

main :: Effect Unit
main = void $ runInBody grid
