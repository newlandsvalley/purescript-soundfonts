module Audio.SoundFont (
    AUDIO
  , AudioBuffer
  , Instrument
  , MidiNote
  , SoundFont
  , canPlayOgg
  , isWebAudioEnabled
  , setNoteRing
  , logLoadResource
  , loadInstrument
  , loadInstruments
  , loadRemoteSoundFonts
  , loadPianoSoundFont
  , playNote
  , playNotes
  ) where

import Prelude (Unit, bind, id, map, pure, show, ($), (<>), (<<<))
import Control.Monad (liftM1)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, Fiber, launchAff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Array (head, index, reverse)
import Data.Map (Map, lookup, empty)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse, sequenceDefault)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName)
import Audio.SoundFont.Gleitz (RecordingFormat(..), SoundFontType(..), gleitzUrl)
import Audio.SoundFont.Decoder (midiJsToNoteMap, debugNoteIds)

-- | The SoundFont API which we will expose

-- | Audio Effect
foreign import data AUDIO :: Effect

-- | the Audio Buffer for a single note
foreign import data AudioBuffer :: Type

-- | the instrument soundfont
-- | a mapping between MIDI pitch and the note's AudioBuffer
type SoundFont = Map Int AudioBuffer

-- | an instrument name attached to its SoundFont
type Instrument = Tuple InstrumentName SoundFont

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

-- | can the browser play ogg format ?
foreign import canPlayOgg
  :: forall eff. (Eff (au :: AUDIO | eff) Boolean)

-- | is the browser web-audio enabled ?
foreign import isWebAudioEnabled
  :: forall eff. (Eff (au :: AUDIO | eff) Boolean)

-- | setting for how long the note 'rings' after it's alloted time
-- | in order to support features such as a more legato feel
-- | This should be a number between 0 (no ring) and 1 (double the original
-- | note duration)
foreign import setNoteRing
  :: forall eff. Number -> Eff (au :: AUDIO | eff) Unit

-- | load a bunch of soundfonts from the Gleitzmann server
loadRemoteSoundFonts :: ∀ e.
  Array InstrumentName
  -> Aff
     ( ajax :: AJAX
     , au :: AUDIO
     | e
     )
     (Array Instrument)
loadRemoteSoundFonts =
  loadInstruments Nothing

-- | load the piano soundfont from a relative directory on the local server
loadPianoSoundFont :: ∀ e.
  String
  -> Aff
     ( ajax :: AJAX
     , au :: AUDIO
     | e
     )
     Instrument
loadPianoSoundFont localDir =
  loadInstrument (Just localDir) AcousticGrandPiano

-- | load a single instrument SoundFont
-- | The options are to load the soundfont from:
-- |   Benjamin Gleitzman's server (default)
-- |   A directory from the local server if this is supplied
loadInstrument :: ∀ e.
  Maybe String
  -> InstrumentName
  -> Aff
     ( ajax :: AJAX
     , au :: AUDIO
     | e
     )
     Instrument
loadInstrument maybeLocalDir instrumentName = do
  recordingFormat <- liftEff prefferedRecordingFormat
  let
    url =
      case maybeLocalDir of
        Just localDir ->
          localUrl instrumentName localDir recordingFormat
        _ ->
          gleitzUrl instrumentName MusyngKite recordingFormat
  res <- affjax $ defaultRequest { url = url, method = Left GET }
  let
    ejson = midiJsToNoteMap instrumentName res.response
    noteMap = either (\_ -> empty) id ejson
  font <- traverse decodeAudioBuffer noteMap
  pure (Tuple instrumentName font)

-- | load a bunch of instrument SoundFonts (in parallel)
-- | again with options to load either locally or remotely
-- | from Benjamin Gleitzman's server
loadInstruments :: ∀ e.
  Maybe String
  -> Array InstrumentName
  -> Aff
     ( ajax :: AJAX
     , au :: AUDIO
     | e
     )
     (Array Instrument)
loadInstruments maybeLocalDir instrumentNames =
  sequential $ traverse (\name -> parallel (loadInstrument maybeLocalDir name)) instrumentNames

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
playNote :: forall eff. Array Instrument -> MidiNote -> Eff (au :: AUDIO | eff) Number
playNote instruments note =
  let
    maybeInstrument = index instruments note.channel
  in
    case maybeInstrument of
      Just (Tuple name soundfont) ->
        case lookup note.id soundfont of
          Just b -> playFontNote $ fontNote b note
          _ -> pure 0.0
      _ -> pure 0.0

-- | play a bunch of notes asynchronously
-- | return the duration of the phrase
-- | (i.e. the time offset plus duration of the last note in the phrase)
playNotes :: forall eff. Array Instrument -> Array MidiNote -> Eff (au :: AUDIO | eff) Number
playNotes instruments notes =
  let
    pns = map (playNote instruments) notes
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

-- | use OGG if we can, otherwise default to MP3
prefferedRecordingFormat :: ∀ eff. (Eff (au :: AUDIO | eff) RecordingFormat)
prefferedRecordingFormat =
  liftM1 (\b -> if b then OGG else MP3) canPlayOgg

-- | turn a MIDI note (and audio buffer) into a font note (suitable for JS)
fontNote :: AudioBuffer -> MidiNote -> FontNote
fontNote buffer n =
  {
    buffer : buffer
  , timeOffset : n.timeOffset
  , duration : n.duration
  , gain : n.gain
  }

-- | build a local URL where the instrument font is contained
-- | in a resource container described by localDir
localUrl :: InstrumentName -> String -> RecordingFormat -> String
localUrl instrument localDir format =
  localDir <> "/" <> (gleitzmanName instrument) <> "-" <> (show format) <> ".js"
