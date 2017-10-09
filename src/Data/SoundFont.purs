module Data.SoundFont (
    AUDIO
  , AudioBuffer
  , MidiNote
  , logLoadResource
  , loadInstrument
  , loadInstruments
  , playNote
  , playNotes
  ) where

import Prelude (Unit, bind, id, map, pure, show, ($), (<>), (<<<))
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, Fiber, launchAff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Parallel (parallel, sequential, parTraverse_)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Array (head, reverse)
import Data.Map (Map, lookup, empty)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse, sequenceDefault)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.SoundFont.Gleitz (InstrumentName, RecordingFormat(..), SoundFontType(..), gleitzUrl)
import Data.SoundFont.Decoder (NoteMap, midiJsToNoteMap, debugNoteIds)

-- | The SoundFont API which we will expose

-- | Audio Effect
foreign import data AUDIO :: Effect

-- | the Audio Buffer for a single note
foreign import data AudioBuffer :: Type

-- | the instrument fonts
-- | a mapping between MIDI pitch and the note's AudioBuffer
type Instrument = Map Int AudioBuffer

-- | the mapping of an instrument name to its SoundFont
type InstrumentMap = Tuple InstrumentName Instrument

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
  , duration :: Number       -- the duration of the note (sec)
  , gain :: Number           -- the volume (between 0 and 1)
  }

-- | load a bunch of instrument SoundFonts (in parallel)
loadInstrument :: ∀ e.
  InstrumentName
  -> Aff
     ( ajax :: AJAX
     , au :: AUDIO
     | e
     )
     InstrumentMap
loadInstrument instrumentName =
  let
    url = gleitzUrl instrumentName MusyngKite OGG
  in
    do
      res <- affjax $ defaultRequest { url = url, method = Left GET }
      let
        ejson = midiJsToNoteMap instrumentName res.response
        noteMap = either (\_ -> empty) id ejson
      instrument <- traverse decodeAudioBuffer noteMap
      pure (Tuple instrumentName instrument)

-- | load a single instrument SoundFont
loadInstruments :: ∀ e.
  Array InstrumentName
  -> Aff
     ( ajax :: AJAX
     , au :: AUDIO
     | e
     )
     (Array InstrumentMap)
loadInstruments instrumentNames =
  sequential $ traverse (\name -> parallel (loadInstrument name)) instrumentNames

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

-- | play a single note through its soundfont buffer
playNote :: forall eff. Instrument -> MidiNote -> Eff (au :: AUDIO | eff) Number
playNote instrument note =
  case lookup note.id instrument of
    Just b -> playFontNote $ fontNote b note
    _ -> pure 0.0

-- | play a bunch of notes asynchronously
-- | return the duration of the phrase
-- | (i.e. the time offset plus duration of the last note in the phrase)
playNotes :: forall eff. Instrument -> Array MidiNote -> Eff (au :: AUDIO | eff) Number
playNotes instrument notes =
  let
    pns = map (playNote instrument) notes
  in
    map lastDuration (sequenceDefault pns)

-- | return the overall duration of the last note played from a sequence of notes
lastDuration :: Array Number -> Number
lastDuration fs =
  let
    last = head $ reverse fs
  in
    fromMaybe 0.0 last

-- | just for debug
logLoadResource  :: ∀ e.
  InstrumentName ->
  Eff (ajax :: AJAX, console :: CONSOLE | e) (Fiber (ajax :: AJAX, console :: CONSOLE | e) Unit)
logLoadResource instrument =
  let
    url = gleitzUrl instrument MusyngKite OGG
  in
    launchAff $ do
      res <- affjax $ defaultRequest { url = url, method = Left GET }

      let
        ejson = midiJsToNoteMap instrument res.response
      liftEff $ log $ "extract JSON: " <> (either show (debugNoteIds) ejson)

-- | turn a MIDI note (and audio buffer) into a font note (suitable for JS)
fontNote :: AudioBuffer -> MidiNote -> FontNote
fontNote buffer n =
  {
    buffer : buffer
  , timeOffset : n.timeOffset
  , duration : n.duration
  , gain : n.gain
  }
