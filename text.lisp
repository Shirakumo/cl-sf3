#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

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

(define-accessors color-option r g b)
(define-accessors size-option size)
(define-accessors heading-option level)
(define-accessors link-option address)
(define-accessors target-option address)
(define-accessors text markup text)
