# Purescript SoundFonts Guide

The idea of ```soundfonts``` is to allow music to be played in the browser in the most basic way possble.  It allows you to play a note of a specified pitch and volume for a given duration on a particular MIDI instrument.  From this simple starting point, more complicated melodies may be built up. 

The [MIDI 1.0 Specification](https://www.midi.org/specifications/midi1-specifications) enumerates a set of 128 different instruments.  If you were to sample one of these instruments over the range of notes that it can play and then digitise the result, you would produce a ```soundfont```.  Luckily, this has been done already in Benjamin Gleitzman's [collection](https://github.com/gleitz/midi-js-soundfonts).

When you use ```soundfonts``` you start by downloading the data for your chosen instruments, which may be from the original Gleitzman site or a different one if you prefer to host it yourself.  This data is then converted into a set of web-audio [AudioBuffers](https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer) - one for each note.  This buffer set is then associated with the instrument name (enumerated in [purescript-midi](https://github.com/newlandsvalley/purescript-midi)) and as a result an ```Instrument``` type is produced.  These Instruments are saved in an array,

```soundfonts``` adopts the MIDI definition of a note's [pitch](https://newt.phys.unsw.edu.au/jw/notes.html) and volume (a number between 0 and 1) however it differs from MIDI in that rather than using a ```NoteOn``` followed by a ```NoteOff``` message, it uses a duration (in seconds):

```purs 
type MidiNote =
  { channel :: Int -- the MIDI channel
  , id :: Int -- the MIDI pitch number
  , timeOffset :: Number -- the time delay in seconds before the note is played
  , duration :: Number -- the duration of the note
  , gain :: Number -- the volume (between 0 and 1)
  }
```

Here the ```channel``` in which the note plays is synonymous with the index into the Instrument array. Note that the note is scheduled to play after the ```timeOffset``` has elapsed.

## Dependencies

Nowadays browsers insist that ```web-audio``` cannot be used until the user has made a gesture on the page (such as hitting a ```play``` button).  The minimal set of dependencies required to use ```soundfonts```, using ```web-``` functions to do this is as follows:

```purs 
dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "exceptions"
  , "maybe"
  , "midi"
  , "newtype"
  , "prelude"
  , "soundfonts"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
```

## Playing a Note 

The following example plays the note ```A``` on an acoustic grand piano with no delay for a duration of a second.

```purs
module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, Fiber, launchAff, delay)
import Effect.Exception (throw)
import Effect.Console (log)
import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Audio.SoundFont (Instrument, MidiNote, loadRemoteSoundFonts, midiNote, playNote, playNotes)
import Data.Midi.Instrument (InstrumentName(..))
import Web.DOM.ParentNode (querySelector)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  -- a user gesture is required before the browser is allowed to use web-audio
  doc <- map toParentNode (window >>= document)
  play <- querySelector (wrap "#play") doc
  case play of
    Just e -> do
      el <- eventListener \_ -> playExample
      addEventListener (wrap "click") el false (unsafeCoerce e :: EventTarget)
    Nothing -> throw "No 'play' button"
  pure unit

playExample :: Effect (Fiber Unit)
playExample = launchAff $ do
  _ <- liftEffect $ log "loading soundfonts"
  instruments <- loadRemoteSoundFonts [AcousticGrandPiano]
  playNotesExample instruments 

playNotesExample :: Array Instrument -> Aff Unit
playNotesExample instruments = do
  _ <- liftEffect $ log "paying note sample A"
  duration_ <- liftEffect $ playNote instruments noteSampleA
  pure unit

noteSampleA :: MidiNote
noteSampleA = midiNote 0 57 0.0 1.0 1.0
```

## Playing Chords

To play a chord, you need to supply an array of ```MidiNote``` at the appropriate pitches where each note is set to play at an identical ```timeOffset```.  Firstly we need to descibe the chord:

```purs
noteSampleA :: MidiNote
noteSampleA = midiNote 0 57 0.0 1.0 1.0

noteSampleCAt :: Number -> MidiNote
noteSampleCAt offset = midiNote 0 60 offset 1.0 1.0

noteSampleEAt :: Number -> MidiNote
noteSampleEAt offset = midiNote 0 64 offset 1.0 1.0

chord :: Array MidiNote 
chord = 
  [ noteSampleA
  , noteSampleCAt 0.0 
  , noteSampleEAt 0.0 
  ]
```

and replace ```playNotesExample``` with this which uses ```playNotes``` to play the note array:

```purs
playNotesExample :: Array Instrument -> Aff Unit
playNotesExample instruments = do
  _ <- liftEffect $ log "paying a chord"
  duration_ <- liftEffect $ playNotes instruments chord
  pure unit
```

## Playing Note Sequences

If you want to play a sequence of notes, then you have to start each note at the correct time offset.  For example, to play the notes of the chord above, but in a legato sequence, you can use this:

```purs
legato :: Array MidiNote 
legato = 
  [ noteSampleA
  , noteSampleCAt 1.0 
  , noteSampleEAt 2.0 
  ]
```

Notice that each succeding note starts at the accumulated time offset of the note sequence that has preceded it.  You again supply this array to ```playNotes```:

```purs
playNotesExample :: Array Instrument -> Aff Unit
playNotesExample instruments = do
  _ <- liftEffect $ log "paying legato"
  duration_ <- liftEffect $ playNotes instruments legato
  pure unit
```
## More Complex Melodies 

It quickly becomes cumbersome to describe an entire tune as a simple ```MidiNote``` array.  It is much more convenient to break it up into phrases.  For this reason, the ```Melody``` module defines the ```MidiNote``` array that we have already seen as a ```MidiPhrase``` and an array of such phrases as a ```Melody```. Each MidiPhrase has time offsets relative to the start of the phrase (usually zero for the first note). 

Remember that when you play through ```web-audio``` the sound is generated asynchronously.  This means that if you need to play an entire melody and have it paced properly, you need to invoke a delay for the duration of each phrase after it has played in order that the next phrase starts at the correct time.  The ```Melody``` module defines two functions that handle the pacing in this way - ```playPhrase``` and ```playMelody```.

We can thus invoke the ```legato``` example using ```playPhrase``` instead of ```playNotes```.  First, import from ```Melody```:

```purs
import Audio.SoundFont.Melody (Melody, playPhrase, playMelody)
```

and then use: 

```purs
  duration_ <- playPhrase instruments legato
```

Note that ```playPhrase``` runs directly in ```Aff``` and not ```Effect``` because it needs access to Aff's ```delay``` function.  Nevertheless, the result is identical, apart from the fact that the main thread of execution is suspended until the playback is complete.

Finally, if we also import ```Data.Unfoldable (replicate)```, we can play a melody of the repeated basic three note phrase:

```purs
  duration_ <- playMelody instruments (replicate 3 legato)
```
 