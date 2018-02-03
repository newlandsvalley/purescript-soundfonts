purescript-soundfonts
=====================

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-soundfonts.svg)](https://github.com/newlandsvalley/purescript-soundfonts/releases)
[![Build Status](https://travis-ci.org/newlandsvalley/purescript-soundfonts.svg?branch=master)](https://travis-ci.org/newlandsvalley/purescript-soundfonts)

This library allows you to play music directly in the browser using a variety of instruments.  These are provided, courtesy of Benjamin Gleitzman's package of [pre-rendered sound fonts](https://github.com/gleitz/midi-js-soundfonts). You can either load them remotely from the Gleitzman GitHub account or else host then locally on your own server.  They load as dictionaries of AudioBuffers - one for each note on each selected instrument. Once loaded. you can play individual notes or groups of notes on your instruments of choice.

Also included is conversion to a __Melody__ type which is suitable for use in a soundfont player widget.

The conversion of a Gleitzman note name (e.g. Ab4) to a MIDI pitch uses middle C = C4.

### Acknowledgements

The design in very heavily influenced by danigb's JavaScript soundfont project: [soundfont-player](https://github.com/danigb/soundfont-player) and in fact initial versions of this library simply wrapped his. However, this version minimises the amount of native JavaScript which is still necessary in order to wrap Web-Audio functions.  These are used in the following places: 

*  When building an AudioBuffer for each note in an instrument SoundFont.
*  When playing a note that uses an AudioBuffer.
*  In functions canPlayOgg and isWebAudioEnabled.

## Build

    bower install   
    pulp build


