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

(bs:define-io-structure archive
  (count uint64)
  (meta-size uint64)
  (meta-offsets (vector uint64 (bs:slot count)))
  (meta-entries (vector archive-meta-entry (bs:slot count) (aref (bs:slot meta-offsets) bs:i)))
  (file-offsets (vector uint64 (bs:slot count)))
  (files (vector (vector uint8 uint64) (bs:slot count) (aref (bs:slot file-offsets) bs:i))))

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
  (format format)
  (samples (vector (case (bs:slot format)
                     (#x01 sint8)
                     (#x02 sint16)
                     (#x04 sint32)
                     (#x08 sint64)
                     (#x11 uint8)
                     (#x12 uint16)
                     (#x14 uint32)
                     (#x18 uint64)
                     (#x22 float16)
                     (#x24 float32)
                     (#x28 float64)))))

(bs:define-io-structure image
  (width uint32)
  (height uint32)
  (depth uint32)
  (channels uint8)
  (format format)
  (data (vector uint8 (* (bs:slot width)
                         (bs:slot height)
                         (bs:slot depth)
                         (bs:slot channels)
                         (bs:octet-size (bs:slot format))))))

(bs:define-io-structure log-entry
  (size uint32)
  (time uint64)
  (severity int8)
  (source (string uint8))
  (category (string uint8))
  (message (string uint16)))

(bs:define-io-structure log-chunk
  (size uint64)
  (entry-count uint32)
  (entry-offsets (vector uint64 (bs:slot entry-count)))
  (entries (vector log-entry (bs:slot entry-count) (aref (bs:slot entry-offsets) bs:i))))

(bs:define-io-structure log
  (start-time uint64)
  (chunk-count uint16)
  (chunks (vector log-chunk (bs:slot chunk-count))))

(bs:define-io-structure model
  (format uint8)
  (material-type uint8)
  (material-size uint32)
  (face-count uint32)
  (textures (vector (string uint16) (ldb (byte 4 0) (bs:slot material-type))))
  (face-indices (vector uint32 (bs:slot face-count)))
  (vertices (vector float32)))

(bs:define-io-structure text
  (encoding (case uint8
              (#x00 :ascii)
              (#x01 :utf-8)
              (#x11 :utf-16)
              (#x21 :utf-32)
              (#x02 :iso-8859-1)
              (#x12 :iso-8859-2)
              (#x22 :iso-8859-3)
              (#x32 :iso-8859-4)
              (#x42 :iso-8859-5)
              (#x52 :iso-8859-6)
              (#x62 :iso-8859-7)
              (#x72 :iso-8859-8)
              (#x82 :iso-8859-9)
              (#x92 :iso-8859-10)
              (#xa2 :iso-8859-11)
              (#xb2 :iso-8859-12)
              (#xc2 :iso-8859-13)
              (#xd2 :iso-8859-14)
              (#xe2 :iso-8859-15)
              (#xf2 :iso-8859-16)
              (#x03 :windows-874)
              (#x13 :windows-1250)
              (#x23 :windows-1251)
              (#x33 :windows-1252)
              (#x43 :windows-1253)
              (#x53 :windows-1254)
              (#x63 :windows-1255)
              (#x73 :windows-1256)
              (#x04 :euc-cn)
              (#x14 :euc-jp)
              (#x24 :euc-kr)
              (#x34 :euc-tw)
              (#x05 :sjis)
              (#x15 :big5)
              (#x25 :gbk)))
  (text (string * (bs:slot encoding))))

(bs:define-io-structure color
  (r float32)
  (g float32)
  (b float32)
  (a float32))

(bs:define-io-structure point
  (x float32)
  (y float32))

(bs:define-io-structure size
  (w float32)
  (h float32))

(bs:define-io-structure line
  (color color)
  (thickness float32)
  (points (vector point uint16)))

(bs:define-io-structure filled-shape
  (fill-color color)
  (outline-color color)
  (outline-thickness float32))

(bs:define-io-structure rectangle
  (:include filled-shape)
  (location point)
  (size size))

(bs:define-io-structure circle
  (:include filled-shape)
  (location point)
  (size size))

(bs:define-io-structure polygon
  (:include filled-shape)
  (location point)
  (points (vector point uint16)))

(bs:define-io-structure curve
  (:include filled-shape)
  (location point)
  (points (vector point uint16)))

(bs:define-io-structure text-shape
  (color color)
  (location point)
  (font (string uint16))
  (font-size float32)
  (text (string uint16)))

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
