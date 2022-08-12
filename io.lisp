#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(defstruct (io
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)
            (:conc-name NIL))
  (start 0 :type (unsigned-byte 64))
  (end 0 :type (unsigned-byte 64))
  (offset start :type (unsigned-byte 64)))

(defstruct (pointer-io
            (:include io)
            (:constructor make-pointer-io (pointer start end))
            (:conc-name NIL))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct (mmap-io
            (:include pointer-io)
            (:constructor make-mmap-io (pointer start end fd))
            (:conc-name NIL))
  (fd 0 :read-only T))

(defun ref-type-ptr-type (ref-type)
  (ecase ref-type
    (:float :float)
    (:double :double)
    (:uint8 :uint8)
    (:uint16 :uint16)
    (:uint32 :uint32)
    (:uint64 :uint64)
    (:int8 :int8)
    (:int16 :int16)
    (:int32 :int32)
    (:int64 :int64)))

(define-type-info pointer-info pointer-io
  (:fields () '(offset pointer))
  (:epilogue (var) `(setf (offset ,var) offset))
  (:ref (var ref-type) `(prog1 (cffi:mem-ref (cffi:inc-pointer pointer offset) ,(ref-type-ptr-type ref-type))
                          (incf offset ,(ref-type-size ref-type))))
  (:ref* (var ref-type) `(cffi:mem-ref (cffi:inc-pointer pointer offset) ,(ref-type-ptr-type ref-type)))
  (:set (var ref-type value) `(prog1 (setf (cffi:mem-ref (cffi:inc-pointer pointer offset) ,(ref-type-ptr-type ref-type)) ,value)
                                (incf offset ,(ref-type-size ref-type))))
  (:pos (var) 'offset))

(defstruct (vector-io
            (:include io)
            (:constructor make-vector-io (vector start end))
            (:conc-name NIL))
  (vector #() :type (simple-array (unsigned-byte 8) (*)) :read-only T))

(declaim (inline sb8ref))
(defun sb8ref (vector offset)
  (let ((byte (aref vector offset)))
    (if (<= byte 127)
        byte
        (- byte 256))))

(declaim (inline (setf sb8ref)))
(defun (setf sb8ref) (byte vector offset)
  (setf (aref vector offset) (if (<= byte 127)
                                 byte
                                 (- 127 byte))))

(defun ref-type-vector-accessor (ref-type)
  (ecase ref-type
    (:float 'nibbles:ieee-single-ref/le)
    (:double 'nibbles:ieee-double-ref/le)
    (:uint8 'aref)
    (:uint16 'nibbles:ub16ref/le)
    (:uint32 'nibbles:ub32ref/le)
    (:uint64 'nibbles:ub64ref/le)
    (:int8 'sb8ref)
    (:int16 'nibbles:sb16ref/le)
    (:int32 'nibbles:sb32ref/le)
    (:int64 'nibbles:sb64ref/le)))

(define-type-info vector-info vector-io
  (:fields () '(offset vector))
  (:epilogue (var) `(setf (offset ,var) offset))
  (:ref (var ref-type) `(prog1 (,(ref-type-vector-accessor ref-type) vector offset)
                          (incf offset ,(ref-type-size ref-type))))
  (:ref* (var ref-type) `(,(ref-type-vector-accessor ref-type) vector offset))
  (:set (var ref-type value) `(prog1 (setf (,(ref-type-vector-accessor ref-type) vector offset) ,value)
                                (incf offset ,(ref-type-size ref-type))))
  (:pos (var) 'offset))

(declaim (inline %file-position (setf %file-position)))
(defun %file-position (stream) (file-position stream))
(defun (setf %file-position) (value stream) (file-position stream value))

(declaim (inline read-sb8))
(defun read-sb8 (stream)
  (let ((byte (read-byte stream)))
    (if (<= byte 127)
        byte
        (- byte 256))))

(declaim (inline write-sb8))
(defun write-sb8 (byte stream)
  (if (<= byte 127)
      (write-byte byte stream)
      (write-byte (- 127 byte) stream)))

(defun ref-type-stream-reader (ref-type)
  (ecase ref-type
    (:float 'nibbles:read-ieee-single/le)
    (:double 'nibbles:read-ieee-double/le)
    (:uint8 'read-byte)
    (:uint16 'nibbles:read-ub16/le)
    (:uint32 'nibbles:read-ub32/le)
    (:uint64 'nibbles:read-ub64/le)
    (:int8 'read-sb8)
    (:int16 'nibbles:read-sb16/le)
    (:int32 'nibbles:read-sb32/le)
    (:int64 'nibbles:read-sb64/le)))

(defun ref-type-stream-writer (ref-type)
  (ecase ref-type
    (:float 'nibbles:write-ieee-single/le)
    (:double 'nibbles:write-ieee-double/le)
    (:uint8 'write-byte)
    (:uint16 'nibbles:write-ub16/le)
    (:uint32 'nibbles:write-ub32/le)
    (:uint64 'nibbles:write-ub64/le)
    (:int8 'write-sb8)
    (:int16 'nibbles:write-sb16/le)
    (:int32 'nibbles:write-sb32/le)
    (:int64 'nibbles:write-sb64/le)))

(define-type-info file-stream-info file-stream
  (:ref (var ref-type) `(,(ref-type-stream-reader ref-type) ,var))
  (:ref* (var ref-type) `(,(ref-type-stream-reader ref-type) ,var))
  (:set (var ref-type value) `(,(ref-type-stream-writer ref-type) ,value ,var))
  (:pos (var) `(%file-position ,var)))

(defmacro define-io-fun (name args &body body)
  `(define-dispatched-fun ,name (pointer-io vector-io file-stream) ,args
     ,@body))

(declaim (inline parse-string))
(defun parse-string (io size)
  (etypecase io
    (pointer-io
     (cffi:foreign-string-to-lisp
      (pointer io) :offset (offset io) :count size :encoding :utf-8))
    (vector-io
     (babel:octets-to-string (vector io) :start (offset io) :end (+ (offset io) size) :encoding :utf-8))
    (file-stream
     (let ((buf (make-array size :element-type 'character)))
       (declare (dynamic-extent buf))
       (read-sequence buf io)
       (babel:octets-to-string buf :encoding :utf-8)))))

(declaim (inline write-string))
(defun write-string (io value)
  ;; This sucks
  (etypecase io
    (pointer-io
     (cffi:lisp-string-to-foreign value (pointer io) most-positive-fixnum :offset (offset io) :encoding :utf-8))
    (vector-io
     (replace (vector io) (babel:string-to-octets value :encoding :utf-8)
              :start1 (offset io)))
    (file-stream
     (write-sequence (babel:string-to-octets value :encoding :utf-8) io))))
