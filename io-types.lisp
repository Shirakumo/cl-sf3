#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(bs:define-io-structure archive-meta-entry
  (modification-time uint64)
  (checksum uint32)
  (mime-type (string uint8))
  (path (string uint16)))

(define-print-method archive-meta-entry "~a ~a" mime-type path)

(bs:define-io-structure archive
  (count uint64)
  (meta-size uint64)
  (meta-offsets (vector uint64 (bs:slot count)))
  (meta-entries (vector archive-meta-entry (bs:slot count) (aref (bs:slot meta-offsets) bs:i)))
  (file-offsets (vector uint64 (bs:slot count)) :offset (+ 8 8 (bs:slot meta-size)))
  (files (vector (vector uint8 uint64) (bs:slot count) (aref (bs:slot file-offsets) bs:i))))

(define-print-method archive "~d" count)

(bs:define-io-alias format
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
  (sample-format format)
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

(bs:define-io-structure image
  (width uint32)
  (height uint32)
  (depth uint32)
  (channels uint8)
  (format format)
  (data (vector uint8 (* (bs:slot width)
                         (bs:slot height)
                         (bs:slot depth)
                         (ldb (byte 4 0) (bs:slot channels))
                         (bs:octet-size (bs:slot format))))))

(defun pixel-type (image)
  (ecase (image-channels image)
    (#x01 :K)
    (#x02 :KA)
    (#x03 :RGB)
    (#x04 :RGBA)
    (#x12 :AK)
    (#x13 :BGR)
    (#x14 :ABGR)
    (#x24 :ARGB)
    (#x34 :BGRA)
    (#x44 :CMYK)
    (#x54 :KYMC)))

(define-print-method image "~d~[~;~:;~:*x~d~]~[~;~:;~:*x~d~] ~a ~a" 
  width height depth format (pixel-type object))

(bs:define-io-structure log-entry
  (size uint32)
  (time uint64)
  (severity sint8)
  (source (string uint8))
  (category (string uint8))
  (message (string uint16)))

(define-print-method log-entry "~a ~d ~a ~a ~a"
  (format-time (log-entry-time object)) severity source category message)

(bs:define-io-structure log-chunk
  (size uint64)
  (entry-count uint32)
  (entry-offsets (vector uint64 (bs:slot entry-count)))
  (entries (vector log-entry (bs:slot entry-count) (aref (bs:slot entry-offsets) bs:i))))

(define-print-method log-chunk "~d" entry-count)

(bs:define-io-structure log
  (start-time uint64)
  (chunk-count uint16)
  (chunks (vector log-chunk (bs:slot chunk-count))))

(define-print-method log "~a ~d" (format-time (log-start-time object)) chunk-count)

(bs:define-io-structure model
  (format uint8)
  (material-type uint8)
  (material-size uint32)
  (textures (vector (string uint16) (ldb (byte 4 0) (bs:slot material-type))))
  (faces (vector uint32 uint32) :offset (+ (bs:slot material-size) 6))
  (vertices (vector float32 uint32)))

(defun vertex-attributes (model)
  (ecase (model-format model)
    (#x03 '(:position))
    (#x05 '(:position :uv))
    (#x06 '(:position :color))
    (#x16 '(:position :normal))
    (#x08 '(:position :uv :normal))
    (#x09 '(:position :color :normal))
    (#x0B '(:position :uv :normal :tangent))
    (#x0C '(:position :color :normal :tangent))))

(defun texture-types (model)
  (ecase (model-material-type model)
    (#x00 '())
    (#x01 '(:albedo))
    (#x02 '(:albedo :normal))
    (#x12 '(:albedo :emission))
    (#x03 '(:albedo :normal :specular))
    (#x13 '(:albedo :normal :emission))
    (#x23 '(:albedo :normal :metallic))
    (#x04 '(:albedo :normal :metalness :roughness))
    (#x14 '(:albedo :normal :specular :emission))
    (#x24 '(:albedo :normal :metallic :emission))
    (#x05 '(:albedo :normal :metalness :roughness :emission))
    (#x15 '(:albedo :normal :metalness :roughness :occlusion))
    (#x06 '(:albedo :normal :metalness :roughness :occlusion :emission))))

(define-print-method model "~a ~d" (vertex-attributes object) (length (model-vertices object)))

(bs:define-io-structure color-option
  (r float32)
  (g float32)
  (b float32))

(define-print-method color-option "~,2f ~,2f ~,2f" r g b)

(bs:define-io-structure size-option
  (size float32))

(define-print-method size-option "~,2f" size)

(bs:define-io-structure heading-option
  (level uint8))

(define-print-method heading-option "~d" level)

(bs:define-io-structure link-option
  (address (string uint16)))

(define-print-method link-option "~a" address)

(bs:define-io-structure target-option
  (address (string uint16)))

(define-print-method target-option "~a" address)

(bs:define-io-alias markup-option
  (case uint8
    (#x01 :bold)
    (#x02 :italic)
    (#x03 :underline)
    (#x04 :strike)
    (#x05 :mono)
    (#x06 color-option)
    (#x07 size-option)
    (#x08 heading-option)
    (#x09 link-option)
    (#x0A target-option)))

(bs:define-io-structure text
  (markup-size uint64)
  (markup-options (vector markup-option uint32))
  (text (string uint64) :offset (+ 8 (bs:slot markup-size))))

(define-print-method text "~a" text)

(bs:define-io-structure color
  (r float32)
  (g float32)
  (b float32)
  (a float32))

(define-print-method color "~,2f ~,2f ~,2f ~,2f" r g b a)

(bs:define-io-structure point
  (x float32)
  (y float32))

(define-print-method point "~,2f ~,2f" x y)

(bs:define-io-structure size
  (w float32)
  (h float32))

(define-print-method size "~,2f ~,2f" w h)

(bs:define-io-structure line
  (color color)
  (thickness float32)
  (points (vector point uint16)))

(define-print-method line "~f ~d" thickness (length (line-points object)))

(bs:define-io-structure filled-shape
  (fill-color color)
  (outline-color color)
  (outline-thickness float32))

(bs:define-io-structure rectangle
  (:include filled-shape)
  (location point)
  (size size))

(define-print-method rectangle "~a ~a" location size)

(bs:define-io-structure circle
  (:include filled-shape)
  (location point)
  (size size))

(define-print-method circle "~a ~a" location size)

(bs:define-io-structure polygon
  (:include filled-shape)
  (points (vector point uint16)))

(define-print-method polygon "~d" (length (polygon-points object)))

(bs:define-io-structure curve
  (:include filled-shape)
  (points (vector point uint16)))

(define-print-method curve "~d" (length (curve-points object)))

(bs:define-io-structure text-shape
  (color color)
  (location point)
  (font (string uint16))
  (font-size float32)
  (text (string uint16)))

(define-print-method text-shape "~a ~a ~,2fun" location font font-size)

(bs:define-io-structure vector-graphic
  (width uint32)
  (height uint32)
  (count uint32)
  (instructions (vector (case uint8
                          (#x01 line)
                          (#x02 rectangle)
                          (#x03 circle)
                          (#x04 polygon)
                          (#x05 curve)
                          (#x06 text-shape)
                          (#x11 #.(make-array 6 :element-type 'single-float :initial-contents
                                              '(1f0 0f0 0f0 0f0 1f0 0f0)))
                          (#x12 (vector float32 6))) 
                        (bs:slot count))))

(define-print-method vector-graphic "~dx~d ~d" width height (vector-graphic-count object))

(bs:define-io-structure sf3-file-header
  #(#x81 #x53 #x46 #x33 #x00 #xE0 #xD0 #x0D #x0A #x0A)
  (type (case uint8
          (1 'archive)
          (2 'audio)
          (3 'image)
          (4 'log)
          (5 'model)
          (6 'text)
          (7 'vector-graphic))))

(bs:define-io-structure sf3-file
  (:include sf3-file-header)
  (content (case (bs:slot type)
             (archive archive)
             (audio audio)
             (image image)
             (log log)
             (model model)
             (text text)
             (vector-graphic vector-graphic))))
