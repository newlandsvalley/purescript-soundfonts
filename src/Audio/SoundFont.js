"use strict";

var sf = function() {

  // warning - none of these initialisers are actually called!
  var context = null;

  // a number in the range 0 (no ring) to 1.0 (full ring)
  var noteRing = 0.1;

  return {
      /* can the browser play ogg format? */
      canPlayOgg : function () {
         var audioTester = document.createElement("audio");
         if (audioTester.canPlayType('audio/ogg')) {
           return true;
         }
         else {
           return false;
         }
      },
      /* Get the audio context */
      establishAudioContext : function() {
        if (sf.context === null || sf.context === undefined) {
          sf.context = new (window.AudioContext || window.webkitAudioContext)();
        }
      },
      /* is web audio enabled ? */
      isWebAudioEnabled : function() {
        sf.establishAudioContext();
        if (sf.context) {
          return true;
        }
        else {
          return false;
        }
      },
      /* Get the current time from the audio context
        I don't think I'll expose this any longer
        it's not needed
      */
      getCurrentTime : function() {
         sf.establishAudioContext();
         if (sf.context) {
           return sf.context.currentTime;
         }
         else {
           return 0;
         }
      },
      /* set the amount that the note 'rings' i.e. exists after its alloted time */
      setNoteRing : function(ring) {
        return function () {
          // console.log('note ring: ', ring);
          sf.noteRing = ring;
        }
      },
      _decodeAudioBuffer : function (psuint8array, onError, onSuccess) {
          sf.establishAudioContext();
          var uint8Array = new Uint8Array(psuint8array);
          if (sf.context) {
             sf.context.decodeAudioData(uint8Array.buffer, function (buff) {
                // console.log('buffer decoded OK ');
                onSuccess(buff)
             }, function (e) {
                // console.log('buffer decode failed ');
                onError('DecodeAudioData error', e);
             })
          }
          else {
            // console.log('no audio context ');
            onError('No audio context');
          }
      },
      /* decode the soundfonts for a particular note */
      decodeAudioBufferImpl : function(uintarray) {
        return function (onError, onSuccess) {
          sf._decodeAudioBuffer (uintarray, onError, onSuccess);
          // Return a canceler, which is just another Aff effect.
          return function (cancelError, cancelerError, cancelerSuccess) {
              cancelerSuccess(); // invoke the success callback for the canceler
          };
        }
       },
      // play a midi note through the appropriate soundfont for the note
      playFontNote :  function (fontNote) {
        return function() {
          return sf._playFontNote(fontNote);
        }
      },
      _playFontNote : function (fontNote) {
        sf.establishAudioContext();
        var source = sf.context.createBufferSource();
        var gainNode = sf.context.createGain();
        var timeOn = sf.context.currentTime + fontNote.timeOffset;
        // I don't know how to initialise noteRing other than on first reference
        if (sf.noteRing == null) {
          sf.noteRing = 0.1;
        }

        // let the note ring for 10% more than it's alloted time to give a more legato feel
        var timeOff = sf.context.currentTime + fontNote.timeOffset + (fontNote.duration * (1.0 + sf.noteRing));
        // var timeOff = sf.context.currentTime + midiNote.timeOffset + midiNote.duration;
        gainNode.gain.value = fontNote.gain;
        source.buffer = fontNote.buffer;
        source.connect(gainNode);
        gainNode.connect(sf.context.destination);
        source.start(timeOn);
        source.stop(timeOff);
        return fontNote.timeOffset + fontNote.duration;
        }
     };
  }();

export var isWebAudioEnabled = sf.isWebAudioEnabled;
export var canPlayOgg = sf.canPlayOgg;
export var setNoteRing = sf.setNoteRing;
export var decodeAudioBufferImpl = sf.decodeAudioBufferImpl;
export var playFontNote = sf.playFontNote;
