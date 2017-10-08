module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.SoundFont.Gleitz (gleitzNoteName)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "gleitz note name Bb0:"
  logShow $ gleitzNoteName "Bb0"
  log "gleitz note name C8:"
  logShow $ gleitzNoteName "C8"
