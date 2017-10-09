module Main where

import Prelude (bind, ($), (*))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Fiber, launchAff, delay)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Data.Time.Duration (Milliseconds(..))
import Audio.SoundFont (AUDIO, MidiNote, logLoadResource, loadInstrument, playNote, playNotes)

note :: Int -> Int -> Number -> Number -> Number -> MidiNote
note channel id timeOffset duration gain =
  { channel : channel, id : id, timeOffset : timeOffset, duration : duration, gain : gain }

noteSampleA :: MidiNote
noteSampleA = note 0 57 0.0 0.5 1.0

noteSampleC :: MidiNote
noteSampleC = note 0 60 0.0 0.5 1.0

noteSampleE :: MidiNote
noteSampleE = note 0 64 0.0 0.5 1.0

notesSample :: Array MidiNote
notesSample =
 [ note 0 60 1.0 0.5 1.0
 , note 0 62 1.5 0.5 1.0
 , note 0 64 2.0 0.5 1.0
 , note 0 65 2.5 0.5 1.0
 , note 0 67 3.0 1.5 1.0
 , note 0 71 3.0 1.5 1.0
 ]

{- just for debug
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
  (Tuple name instrument) <- loadInstrument "marimba"
  da <- liftEff $ playNote instrument noteSampleA
  _ <- delay (Milliseconds $ 1000.0 * da)
  db <- liftEff $ playNote instrument noteSampleC
  _ <- delay (Milliseconds $ 1000.0 * db)
  de <- liftEff $ playNote instrument noteSampleE
  _ <- delay (Milliseconds $ 1000.0 * de)
  liftEff $ playNotes instrument notesSample
  -- pure font
