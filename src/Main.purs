module Main where

import Prelude (bind, ($), (*))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, Fiber, launchAff, delay)
import Network.HTTP.Affjax (AJAX)
import Data.Time.Duration (Milliseconds(..))
import Data.SoundFont (AUDIO, MidiNote, logLoadResource, loadInstrument, playNote)


note :: Int -> Int -> Number -> Number -> Number -> MidiNote
note channel id timeOffset duration gain =
  { channel : channel, id : id, timeOffset : timeOffset, duration : duration, gain : gain }

noteSampleA :: MidiNote
noteSampleA = note 0 57 0.0 0.5 1.0

noteSampleC :: MidiNote
noteSampleC = note 0 60 0.0 0.5 1.0

noteSampleE :: MidiNote
noteSampleE = note 0 64 0.0 0.5 1.0

{-}
main :: ∀ e.
  Eff (ajax :: AJAX, console :: CONSOLE | e) (Fiber (ajax :: AJAX, console :: CONSOLE | e) Unit)
main = do
  _ <- logLoadResource "acoustic_grand_piano"
  logLoadResource "marimba"
-}

main :: ∀ eff.
        Eff
          ( ajax :: AJAX
          , au :: AUDIO
          | eff
          )
          (Fiber
             ( ajax :: AJAX
             , au :: AUDIO
             | eff
             )
             Number
          )
main = launchAff $ do
  -- instrument <- loadInstrument "acoustic_grand_piano"
  instrument <- loadInstrument "marimba"
  da <- liftEff $ playNote noteSampleA instrument
  _ <- delay (Milliseconds $ 1000.0 * da)
  db <- liftEff $ playNote noteSampleC instrument
  _ <- delay (Milliseconds $ 1000.0 * db)
  liftEff $ playNote noteSampleE instrument
  -- pure font
