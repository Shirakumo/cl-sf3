(in-package #:org.shirakumo.sf3)

(bs:define-io-alias sample-format
    (case uint8
      (#x01 'sint8)
      (#x02 'sint16)
      (#x04 'sint32)
      (#x08 'sint64)
      (#x11 'uint8)
      (#x12 'uint16)
      (#x14 'uint32)
      (#x18 'uint64)
      (#x22 'float16)
      (#x24 'float32)
      (#x28 'float64)))

(bs:define-io-structure audio
  (samplerate uint32)
  (channels uint8)
  (sample-format sample-format)
  (frame-count uint64)
  (samples (vector (case (bs:slot format)
                     (#x01 sint8)
                     (#x02 sint16)
                     (#x04 sint32)
                     (#x08 sint64)
                     (#x11 uint8)
                     (#x12 uint16)
                     (#x14 uint32)
                     (#x18 uint64)
                     (#x24 float32)
                     (#x28 float64))
                   (the bs::index (* (bs:slot frame-count) (bs:slot channels))))))

(defun duration (audio)
  (float (/ (audio-frame-count audio)
            (audio-samplerate audio))))

(define-print-method audio "~d:~2,'0d ~dx~a @~dHz"
  (floor (duration object) 60) (mod (ceiling (duration object)) 60)
  channels sample-format samplerate)

(define-accessors audio samplerate channels sample-format samples)
