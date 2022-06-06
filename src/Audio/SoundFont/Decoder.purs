module Audio.SoundFont.Decoder
  ( NoteMap
  , midiJsToNoteMap
  , debugNoteNames
  , debugNoteIds
  ) where

import Prelude ((<>), ($), (+), map, show)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, take, indexOf, lastIndexOf, length)
import Data.Midi.Instrument (InstrumentName, gleitzmanName)
import Audio.SoundFont.Gleitz (debugNoteName, midiPitch)
import Data.Argonaut.Core (Json, caseJsonObject, caseJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object (keys, toUnfoldable) as SM
import Data.Map (Map, fromFoldable, keys)
import Data.Set (toUnfoldable) as Set
import Data.List (List)
import Data.Traversable (sequenceDefault)

import Data.Foldable (intercalate)
import Data.Binary.Base64 (decode) as B64
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Bifunctor (lmap)
import Effect.Exception (Error, error)

-- | This module transforms the MIDI.js from https://github.com/gleitz/midi-js-soundfonts
-- | for a diven instrument, extracts the Json and parses it and also decodes the
-- | Base64 representation  of each note to an unsigned Int8 array.
-- | It then returns a map indexed by MIDI note number for each note in the instrument font.
-- | Note that this note definition is not yet ready to play - it must still be decoded
-- | via a Web-Audio Audio Context.

-- | Argonaut-facing Map
type NoteMap0 = Object Uint8Array

-- | the final note map indexed by the MIDI pitch number
type NoteMap = Map Int Uint8Array

-- | convert a Note Map indexed by Strings representing the notes
-- | to one indexed by Ints (the MIDI Pitch of that note String)
-- | This seems expensive - maybe Argonaut is not the way to go
rebaseNoteMap :: NoteMap0 -> NoteMap
rebaseNoteMap nm =
  let
    -- seem to need the type signature to help the type checker
    intermediate :: Array (Tuple Int Uint8Array)
    intermediate = map (lmap midiPitch) $ SM.toUnfoldable nm
  in
    fromFoldable intermediate

-- | Parse the downloaded MIDI.js and process the JSON if we can.
-- | This MIDI.js format starts with some javascript which we need to ditch
-- | and then the almost-Json for the requested instrument -
-- | unfortunately it leaves a trailing comma before the final brace.
midiJsToNoteMap :: InstrumentName -> String -> Either Error NoteMap
midiJsToNoteMap instrumentName mjs =
  let
    instrument = gleitzmanName instrumentName
    patternString = "MIDI.Soundfont." <> instrument <> " = "
    pattern = Pattern patternString
    posStart = indexOf pattern mjs
    posEnd = lastIndexOf (Pattern ",") mjs
  in
    case (Tuple posStart posEnd) of
      (Tuple (Just start) (Just end)) ->
        let
          text = (drop (start + length patternString) (take end mjs)) <> "}"
        in
          case jsonParser text of
            Left err -> Left $ error err
            Right json -> decodeJson json
      _ ->
        Left $ error $ "Invalid MIDI.js Soundfont format found for " <> instrument

-- | the top level should just be a simple Json Object, with an k-v entry for each note
decodeJson :: Json -> Either Error NoteMap
decodeJson =
  -- foldJsonObject (Left $ error "invalid Json") decodeJObject
  caseJsonObject (Left $ error "invalid Json object") (\jobj -> decodeJObject jobj)

-- | and each object should just hold a string representing the
-- | base64 endcoding of the note
decodeJObject :: Object Json -> Either Error NoteMap
decodeJObject jo =
  map rebaseNoteMap $ sequenceDefault $ map decodeJString jo

decodeJString :: Json -> Either Error Uint8Array
decodeJString =
  caseJsonString (Left $ error "invalid Json string") (\s -> decodeB64 s)

-- | and we need to strip off the preface of each value
-- | to get at the raw base64 which we can then decode
decodeB64 :: String -> Either Error Uint8Array
decodeB64 s =
  let
    pos = lastIndexOf (Pattern ",") s
  in
    case pos of
      Just p ->
        let
          text = drop (p + 1) s
        in
          B64.decode text
      _ -> Left $ error "invalid note definition in Json"

-- debug functions
debugNoteNames :: NoteMap0 -> String
debugNoteNames nm =
  intercalate "," (map debugNoteName $ SM.keys nm)

debugNoteIds :: NoteMap -> String
debugNoteIds nm =
  let
    keyList :: List Int
    keyList = Set.toUnfoldable (keys nm)
  in
    intercalate "," (map show keyList)
