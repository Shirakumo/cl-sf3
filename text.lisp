#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(define-file-type text #x00
  (encoding (map :uint8
                 (#x00 :US-ASCII)
                 (#x01 :UTF-8)
                 (#x11 :UTF-16)
                 (#x21 :UTF-32)
                 (#x02 :ISO-8859-1)
                 (#x12 :ISO-8859-2)
                 (#x22 :ISO-8859-3)
                 (#x32 :ISO-8859-4)
                 (#x42 :ISO-8859-5)
                 (#x52 :ISO-8859-6)
                 (#x62 :ISO-8859-7)
                 (#x72 :ISO-8859-8)
                 (#x82 :ISO-8859-9)
                 (#x92 :ISO-8859-10)
                 (#xA2 :ISO-8859-11)
                 (#xB2 :ISO-8859-12)
                 (#xC2 :ISO-8859-13)
                 (#xD2 :ISO-8859-14)
                 (#xE2 :ISO-8859-15)
                 (#xF2 :ISO-8859-16)
                 (#x03 :Windows-874)
                 (#x13 :Windows-1250)
                 (#x23 :Windows-1251)
                 (#x33 :Windows-1252)
                 (#x43 :Windows-1253)
                 (#x53 :Windows-1254)
                 (#x63 :Windows-1255)
                 (#x73 :Windows-1256)
                 (#x04 :EUC-CN)
                 (#x14 :EUC-JP)
                 (#x24 :EUC-KR)
                 (#x34 :EUC-TW)
                 (#x05 :SJIS)
                 (#x15 :Big5)
                 (#x25 :GBK))))

(defun text-string (text)
  (let ((io (io text)))
    (etypecase io
      (pointer-io
       (cffi:foreign-string-to-lisp
        (pointer io) :offset (+ 11 (start io)) :count (- (end io) (start io) 11)
                     :encoding (text-encoding text)))
      (vector-io
       (babel:octets-to-string
        (vector io) :start (+ 11 (start io)) :end (end io)
                    :encoding (text-encoding text)))
      (file-stream
       ;; FIXME: do it
       ))))
