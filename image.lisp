#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(define-file-type image #x01
  (width :uint32)
  (height :uint32)
  (depth :uint32)
  (channels (map :uint8
                 (#x01 :R)
                 (#x02 :RA)
                 (#x03 :RGB)
                 (#x04 :RGBA)
                 (#x12 :AR)
                 (#x13 :BGR)
                 (#x14 :ABGR)
                 (#x24 :ARGB)
                 (#x34 :BGRA)))
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
               (#x28 :float64)))
  (payload :payload))

(defun image-data (image)
  (let ((io (io image)))
    (etypecase io
      (pointer-io
       (values (cffi:inc-pointer (pointer io) (image-payload image))
               (- (end io) (image-payload image))))
      (vector-io
       (values (vector io)
               (image-payload image)
               (end io)))
      (file-stream
       (file-position io (image-payload image))
       (values io
               (- (file-length io) (image-payload image)))))))
