(in-package #:org.shirakumo.sf3)

(bs:define-io-alias sample-format
    (case uint8
      (#x01 'bs:sint8)
      (#x02 'bs:sint16)
      (#x04 'bs:sint32)
      (#x08 'bs:sint64)
      (#x11 'bs:uint8)
      (#x12 'bs:uint16)
      (#x14 'bs:uint32)
      (#x18 'bs:uint64)
      (#x22 'bs:float16)
      (#x24 'bs:float32)
      (#x28 'bs:float64)))

(bs:define-io-structure (audio (:constructor %make-audio))
  (samplerate uint32)
  (channels uint8)
  (sample-format sample-format)
  (frame-count uint64)
  (samples (vector (case (bs:slot format)
                     (#x01 bs:sint8)
                     (#x02 bs:sint16)
                     (#x04 bs:sint32)
                     (#x08 bs:sint64)
                     (#x11 bs:uint8)
                     (#x12 bs:uint16)
                     (#x14 bs:uint32)
                     (#x18 bs:uint64)
                     (#x22 bs:uint16)
                     (#x24 bs:float32)
                     (#x28 bs:float64))
                   (the bs::index (* (bs:slot frame-count) (bs:slot channels))))))

(defun make-audio (samples &key (samplerate 48000) (channels 1))
  (%make-audio :samplerate samplerate
               :channels channels
               :sample-format (lisp-type->bs-type (array-element-type samples))
               :frame-count (truncate (length samples) channels)
               :samples samples))

(defun duration (audio)
  (float (/ (audio-frame-count audio)
            (audio-samplerate audio))))

(define-print-method audio "~d:~2,'0d ~dx~a @~dHz"
  (floor (duration object) 60) (mod (ceiling (duration object)) 60)
  channels sample-format samplerate)

(define-accessors audio samplerate channels sample-format samples)
