(in-package #:org.shirakumo.sf3)

(bs:define-io-alias sample-format
    (case uint8
      (#x01 :sint8)
      (#x02 :sint16)
      (#x04 :sint32)
      (#x08 :sint64)
      (#x11 :uint8)
      (#x12 :uint16)
      (#x14 :uint32)
      (#x18 :uint64)
      (#x22 :float16)
      (#x24 :float32)
      (#x28 :float64)))

(bs:define-io-structure (audio (:constructor %make-audio))
  (samplerate uint32)
  (channels uint8)
  (sample-format sample-format)
  (frame-count uint64)
  (samples (case (bs:slot sample-format)
             (:sint8 (vector :sint8 (audio-sample-count bs:instance)))
             (:sint16 (vector :sint16 (audio-sample-count bs:instance)))
             (:sint32 (vector :sint32 (audio-sample-count bs:instance)))
             (:sint64 (vector :sint64 (audio-sample-count bs:instance)))
             (:uint8 (vector :uint8 (audio-sample-count bs:instance)))
             (:uint16 (vector :uint16 (audio-sample-count bs:instance)))
             (:uint32 (vector :uint32 (audio-sample-count bs:instance)))
             (:uint64 (vector :uint64 (audio-sample-count bs:instance)))
             (:uint16 (vector :uint16 (audio-sample-count bs:instance)))
             (:float32 (vector :float32 (audio-sample-count bs:instance)))
             (:float64 (vector :float64 (audio-sample-count bs:instance))))))

(defun audio-sample-count (audio)
  (* (audio-frame-count audio)
     (audio-channels audio)))

(defun make-audio (samples &key (samplerate 48000) (channels 1))
  (%make-audio :samplerate samplerate
               :channels channels
               :sample-format (lisp-type->bs-type (array-element-type samples))
               :frame-count (truncate (length samples) channels)
               :samples samples))

(defun duration (audio)
  (float (/ (audio-frame-count audio)
            (audio-samplerate audio))))

(defun channel-layout (audio)
  (ecase (audio-channels audio)
    (1 '(:center-front))
    (2 '(:left-front :right-front))
    (3 '(:left-front :right-front :center-front))
    (4 '(:left-front :right-front :left-rear :right-rear))
    (5 '(:left-front :right-front :left-rear :right-rear :subwoofer))
    (6 '(:left-front :right-front :center-front :left-rear :right-rear :subwoofer))
    (7 '(:left-front :right-front :center-front :left-rear :right-rear :left-side :right-side))
    (8 '(:left-front :right-front :center-front :left-rear :right-rear :left-side :right-side :subwoofer))
    (9 '(:left-front :right-front :center-front :left-rear :center-rear :right-rear :left-side :right-side :subwoofer))))

(define-print-method audio "~d:~2,'0d ~dx~a @~dHz"
  (floor (duration object) 60) (mod (ceiling (duration object)) 60)
  channels sample-format samplerate)

(define-accessors audio samplerate channels sample-format samples)
