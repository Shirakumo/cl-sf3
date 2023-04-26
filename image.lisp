#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(bs:define-io-alias pixel-format
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

(bs:define-io-structure image
  (width uint32)
  (height uint32)
  (depth uint32)
  (channels uint8)
  (format pixel-format)
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

(define-accessors image width height depth channels data)
