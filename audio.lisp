#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(define-file-type audio #x02
  (samplerate :uint32)
  (channels :uint8)
  (format (map :uint8
               (#x01 :int8)
               (#x02 :int16)
               (#x04 :int32)
               (#x08 :int64)
               (#x11 :uint8)
               (#x12 :uint16)
               (#x14 :uint32)
               (#x18 :uint64)
               (#x22 :float16)
               (#x24 :float32)
               (#x28 :float64))))
