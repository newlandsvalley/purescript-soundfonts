module Audio.SoundFont
  ( AudioBuffer
  , Instrument
  , InstrumentChannels
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
  , instrumentChannels
  ) where

import Affjax.Web (defaultRequest, request)
import Affjax.ResponseFormat as ResponseFormat
import Audio.SoundFont.Decoder (midiJsToNoteMap, debugNoteIds)
import Audio.SoundFont.Gleitz (RecordingFormat(..), SoundFontType(..), gleitzUrl)
import Control.Parallel (parallel, sequential)
import Data.Array (index, last, mapWithIndex)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Map (Map, lookup, empty, fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName)
import Data.Traversable (traverse, sequenceDefault)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prelude

-- | The SoundFont API which we will expose

-- | the Audio Buffer for a single note
foreign import data AudioBuffer :: Type

-- | the instrument soundfont
-- | a mapping between MIDI pitch and the note's AudioBuffer
type SoundFont = Map Int AudioBuffer

-- | an instrument name attached to its SoundFont
type Instrument = Tuple InstrumentName SoundFont

-- | the mapping of instrument names to MIDI channels
type InstrumentChannels = Map InstrumentName Int

-- | A Midi Note
type MidiNote =
  { channel :: Int -- the MIDI channel
  , id :: Int -- the MIDI pitch number
  , timeOffset :: Number -- the time delay in seconds before the note is played
  , duration :: Number -- the duration of the note
  , gain :: Number -- the volume (between 0 and 1)
  }

-- | A Midi Note with the appropriate font
type FontNote =
  { buffer :: AudioBuffer -- the Audio buffer for a particular note on a particular instrument
  , timeOffset :: Number -- the time delay in seconds before the note is played
  , duration :: Number -- the duration of the note (sec)
  , gain :: Number -- the volume (between 0 and 1)
  }

-- | can the browser play ogg format ?
foreign import canPlayOgg
  :: Effect Boolean

-- | is the browser web-audio enabled ?
foreign import isWebAudioEnabled
  :: Effect Boolean

-- | setting for how long the note 'rings' after it's alloted time
-- | in order to support features such as a more legato feel
-- | This should be a number between 0 (no ring) and 1 (double the original
-- | note duration)
foreign import setNoteRing
  :: Number -> Effect Unit

-- | load a bunch of soundfonts from the Gleitzmann server
loadRemoteSoundFonts
  :: Array InstrumentName
  -> Aff (Array Instrument)
loadRemoteSoundFonts =
  loadInstruments Nothing

-- | load the piano soundfont from a relative directory on the local server
loadPianoSoundFont
  :: String
  -> Aff Instrument
loadPianoSoundFont localDir =
  loadInstrument (Just localDir) AcousticGrandPiano

-- | load a single instrument SoundFont
-- | The options are to load the soundfont from:
-- |   Benjamin Gleitzman's server (default)
-- |   A directory from the local server if this is supplied

loadInstrument
  :: Maybe String
  -> InstrumentName
  -> Aff Instrument
loadInstrument maybeLocalDir instrumentName = do
  recordingFormat <- liftEffect prefferedRecordingFormat
  let
    url =
      case maybeLocalDir of
        Just localDir ->
          localUrl instrumentName localDir recordingFormat
        _ ->
          gleitzUrl instrumentName MusyngKite recordingFormat
  res <- request $ defaultRequest
    { url = url, method = Left GET, responseFormat = ResponseFormat.string }

  case res <#> _.body of
    Left _ -> do
      _ <- liftEffect $ log $ "instrument failed to load: " <> url
      pure (Tuple instrumentName empty)
    Right body -> do
      let
        ejson = midiJsToNoteMap instrumentName body
        noteMap = either (\_ -> empty) identity ejson
      font <- traverse decodeAudioBuffer noteMap
      pure (Tuple instrumentName font)

-- | load a bunch of instrument SoundFonts (in parallel)
-- | again with options to load either locally or remotely
-- | from Benjamin Gleitzman's server
loadInstruments
  :: Maybe String
  -> Array InstrumentName
  -> Aff (Array Instrument)
loadInstruments maybeLocalDir instrumentNames =
  sequential $ traverse (\name -> parallel (loadInstrument maybeLocalDir name)) instrumentNames

foreign import decodeAudioBufferImpl
  :: Uint8Array -> EffectFnAff AudioBuffer

-- | decode the AudioBuffer for a given note
decodeAudioBuffer :: Uint8Array -> Aff AudioBuffer
decodeAudioBuffer =
  fromEffectFnAff <<< decodeAudioBufferImpl

-- | play a note asynchronously
-- | return the (time offset + duration) of the note
foreign import playFontNote
  :: FontNote -> Effect Number

-- | play a single note through its soundfont buffer.
-- | if the note cannot be played because it:
-- |    uses an invalid instrument
-- |    has a note index that can't be resolved
-- | then treat the note as a Rest, with its stated duration
playNote :: Array Instrument -> MidiNote -> Effect Number
playNote instruments note =
  let
    maybeInstrument = index instruments note.channel
  in
    case maybeInstrument of
      Just (Tuple _ soundfont) ->
        case lookup note.id soundfont of
          -- play the note
          Just b -> playFontNote $ fontNote b note
          -- play nothing for the note duration - this represents a rest
          _ -> pure $ note.timeOffset + note.duration
      _ -> pure $ note.timeOffset + note.duration

-- | play a bunch of notes asynchronously
-- | return the duration of the phrase
-- | (i.e. the time offset plus duration of the last note in the phrase)
playNotes :: Array Instrument -> Array MidiNote -> Effect Number
playNotes instruments notes =
  let
    pns = map (playNote instruments) notes
  in
    map lastDuration (sequenceDefault pns)

-- | create a map of instrument name to channel from an Instrument Array
instrumentChannels :: Array Instrument -> InstrumentChannels
instrumentChannels is =
  let
    f :: Int -> Instrument -> Tuple InstrumentName Int
    f i = rmap (\_ -> i)
  in
    fromFoldable $ mapWithIndex f is

-- | return the overall duration of the last note played from a sequence of notes
lastDuration :: Array Number -> Number
lastDuration ns =
  fromMaybe 0.0 $ last ns

-- | just for debug
logLoadResource
  :: InstrumentName
  -> Effect (Fiber Unit)
logLoadResource instrument =
  let
    url = gleitzUrl instrument MusyngKite OGG
  in
    launchAff $ do
      res <- request $ defaultRequest
        { url = url, method = Left GET, responseFormat = ResponseFormat.string }
      case res <#> _.body of
        Left _ ->
          liftEffect $ log $ "instrument failed to load: " <> url
        Right body -> do
          let
            ejson = midiJsToNoteMap instrument body
          liftEffect $ log $ "extract JSON: " <> (either show (debugNoteIds) ejson)

-- | use OGG if we can, otherwise default to MP3
prefferedRecordingFormat :: Effect RecordingFormat
prefferedRecordingFormat =
  liftM1 (\b -> if b then OGG else MP3) canPlayOgg

-- | turn a MIDI note (and audio buffer) into a font note (suitable for JS)
fontNote :: AudioBuffer -> MidiNote -> FontNote
fontNote buffer n =
  { buffer: buffer
  , timeOffset: n.timeOffset
  , duration: n.duration
  , gain: n.gain
  }

-- | build a local URL where the instrument font is contained
-- | in a resource container described by localDir
localUrl :: InstrumentName -> String -> RecordingFormat -> String
localUrl instrument localDir format =
  localDir <> "/" <> (gleitzmanName instrument) <> "-" <> (show format) <> ".js"
