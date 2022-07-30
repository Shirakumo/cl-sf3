#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(define-parsed-struct archive-entry NIL
  (file-size :uint64)
  (mod-time :uint64)
  (checksum :uint32)
  (mime-type (string :uint8))
  (path (string :uint16)))

(define-file-type archive #x04
  (count :uint64)
  (metadata-size :uint64)
  (entry-offsets (vector count :uint64))
  (metadata (vector count (object archive-entry)))
  (file-offsets (vector count :uint64))
  (payload :payload))

(defun archive-file-data (archive file-index)
  (let* ((io (io archive))
         (start (+ (start io) (aref (archive-file-offsets archive) file-index)))
         (end (+ (start io) (aref (archive-file-offsets archive) (1+ file-index)))))
    (etypecase io
      (pointer-io
       (values (cffi:inc-pointer (pointer io) start) (- start end)))
      (vector-io
       (values (vector io) start end))
      (file-stream
       (file-position io start)
       (values io (- start end))))))
