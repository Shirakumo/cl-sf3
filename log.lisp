#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(define-parsed-struct log-entry NIL
  (size :uint32)
  (time :uint64)
  (severity :int8)
  (source (string :uint8))
  (category (string :uint8))
  (message (string :uint16)))

(define-parsed-struct chunk NIL
  (size :uint64)
  (count :uint32)
  (offsets (vector count :uint64))
  (entries :payload))

(define-file-type log #x05
  (start-time :uint64)
  (chunk-count :uint32)
  (chunk (vector chunk-count (object chunk))))

