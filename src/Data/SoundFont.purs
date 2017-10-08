module Data.SoundFont (
    AUDIO
  , AudioBuffer
  , MidiNote
  , logLoadResource
  , loadInstrument
  , playNote
  )where

import Prelude (Unit, bind, const, id, map, pure, show, ($), (<>), (<<<))
import Control.Monad (liftM1)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, Fiber, launchAff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Map (Map(..), lookup, empty)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.SoundFont.Gleitz (InstrumentName, RecordingFormat(..), SoundFontType(..), gleitzUrl)
import Data.SoundFont.Decoder (NoteMap, midiJsToNoteMap, debugNoteNames, debugNoteIds)


-- | The SoundFont API which we will expose


-- | Audio Effect
foreign import data AUDIO :: Effect

foreign import data AudioBuffer :: Type

-- | the instrument fonts
-- | a mapping between MIDI pitch and the note's AudioBuffer
type Instrument = Map Int AudioBuffer

-- | A Midi Note
type MidiNote =
  { channel :: Int           -- the MIDI channel
  , id  :: Int               -- the MIDI pitch number
  , timeOffset :: Number     -- the time delay in seconds before the note is played
  , duration :: Number       -- the duration of the note
  , gain :: Number           -- the volume (between 0 and 1)
  }

-- | A Midi Note with the appropriate font
type FontNote =
  { buffer  ::  AudioBuffer  -- the Audio buffer for a particular note on a particular instrument
  , timeOffset :: Number     -- the time delay in seconds before the note is played
  , duration :: Number       -- the duration of the note
  , gain :: Number           -- the volume (between 0 and 1)
  }

fontNote :: AudioBuffer -> FontNote
fontNote buffer =
  {
    buffer : buffer
  , timeOffset : 0.1
  , duration : 0.5
  , gain : 1.0
  }


foreign import decodeAudioBufferImpl
  :: forall eff. Uint8Array  -> EffFnAff (au :: AUDIO | eff) AudioBuffer

-- | decode the AudioBuffer for a given note
decodeAudioBuffer :: forall eff. Uint8Array -> Aff (au :: AUDIO | eff) AudioBuffer
decodeAudioBuffer =
  fromEffFnAff <<< decodeAudioBufferImpl

-- | play a note asynchronously
-- | return the (time offset + duration) of the note
foreign import playFontNote
  :: forall eff. FontNote -> Eff (au :: AUDIO | eff) Number

playNote :: forall eff. MidiNote -> Instrument -> Eff (au :: AUDIO | eff) Number
playNote note instrument =
  case lookup note.id instrument of
    Just b -> playFontNote $ fontNote b
    _ -> pure 0.0


logLoadResource  :: ∀ e.
  InstrumentName ->
  Eff (ajax :: AJAX, console :: CONSOLE | e) (Fiber (ajax :: AJAX, console :: CONSOLE | e) Unit)
logLoadResource instrument =
  let
    url = gleitzUrl instrument MusyngKite OGG
  in
    launchAff $ do
      res <- affjax $ defaultRequest { url = url, method = Left GET }
      -- liftEff $ log $ "GET soundfont response: " <> res.response

      let
        ejson = midiJsToNoteMap instrument res.response
      liftEff $ log $ "extract JSON: " <> (either show (debugNoteIds) ejson)

{-}
loadInstrument  :: ∀ e.
  InstrumentName ->
  Eff (ajax :: AJAX | e) (Fiber (ajax :: AJAX | e) NoteMap)
  -}
loadInstrument instrumentName =
  let
    url = gleitzUrl instrumentName MusyngKite OGG
  in
    do
      res <- affjax $ defaultRequest { url = url, method = Left GET }

      let
        ejson = midiJsToNoteMap instrumentName res.response
        noteMap = either (\_ -> empty) id ejson
        -- instrument :: Instrument
      instrument <- traverse decodeAudioBuffer noteMap
      pure instrument
