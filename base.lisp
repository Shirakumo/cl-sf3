#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(defstruct (sf3-file
            (:constructor NIL)
            (:copier NIL)
            (:conc-name NIL))
  (io))

(defgeneric open-sf3 (source &key))
(defgeneric close-sf3 (thing))
(defgeneric write-sf3 (file target &key))
(defgeneric id (file))
(defgeneric parse-for (id io))
(defgeneric write-for (file io))

(defmethod close-sf3 ((file sf3-file))
  (when (io file)
    (close (io file))
    (setf (io file) NIL)))

(defmethod close-sf3 ((source stream))
  (close source))

(defmethod close-sf3 ((source io)))

(defmethod close-sf3 ((source mmap-io))
  (mmap:munmap (pointer source) (fd source) (end source)))

(defmethod open-sf3 ((source pathname) &key (method :mmap))
  (ecase method
    (:stream
     (let ((stream (open source :direction :input :element-type '(unsigned-byte 8))))
       (with-cleanup-on-failure (ignore-errors (close stream))
         (open-sf3 stream))))
    (:mmap
     (multiple-value-bind (addr fd size) (mmap:mmap source)
       (with-cleanup-on-failure (ignore-errors (mmap:munmap addr fd size))
         (open-sf3 (make-mmap-io addr 0 size fd)))))))

(defmethod open-sf3 ((source stream) &key)
  (let ((temp #p""))
    (with-open-file (temp temp :direction :output :element-type '(unsigned-byte 8))
      (let ((buf (make-array 8096 :element-type '(unsigned-byte 8))))
        (declare (dynamic-extent buf))
        (loop for read = (read-sequence buf source)
              while (< 0 read)
              do (write-sequence buf temp :end read))))
    (with-cleanup-on-failure (ignore-errors (delete-file temp))
      (open-sf3 temp))))

(defmethod open-sf3 ((source cl:vector) &key (start 0) (end (length source)))
  (check-type source (simple-array (unsigned-byte 8) (*)))
  (open-sf3 (make-vector-io source start end)))

(defmethod open-sf3 (source &key (start 0) (end most-positive-fixnum))
  (etypecase source
    (cffi:foreign-pointer
     (open-sf3 (make-pointer-io source start end)))))

(defmethod open-sf3 ((source file-stream) &key)
  (parse-sf3 source))

(defmethod open-sf3 ((source io) &key)
  (parse-sf3 source))

(defmacro with-sf3 ((file source &rest args) &body body)
  `(let ((,file (open-sf3 ,source ,@args)))
     (unwind-protect
          (let ((,file ,file))
            ,@body)
       (close-sf3 ,file))))

(defmethod write-sf3 ((file sf3-file) target &key (start 0) (end most-positive-fixnum))
  (etypecase target
    (cffi:foreign-pointer
     (%write-sf3 file (make-pointer-io target start end)))))

(defmethod write-sf3 ((file sf3-file) (target cl:vector) &key (start 0) (end most-positive-fixnum))
  (check-type target (simple-array (unsigned-byte 8) (*)))
  (%write-sf3 file (make-vector-io target start end)))

(defmethod write-sf3 ((file sf3-file) (target pathname) &key (if-exists :error))
  (with-open-file (stream target :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists if-exists)
    (%write-sf3 file stream)))

(define-io-fun parse-sf3 (io)
  (assert (= (ref :uint32) #x33485381)) ;; 81 53 46 33
  (assert (= (ref :uint32) #x0DD0E000)) ;; 00 E0 D0 0D
  (assert (= (ref :uint16) #x0A0A))     ;; 0A 0A
  (parse-for (ref :uint8) io))

(define-io-fun %write-sf3 (io file)
  (set :uint32 #x33485381)
  (set :uint32 #x0DD0E000)
  (set :uint16 #x0A0A)
  (set :uint8 (id file))
  (write-for file io))

(defmethod make ((type integer) io)
  (error "Unknown SF3 file format ID: ~2,'0x" type))

(defmacro define-file-type (name id &body header)
  (let ((parse (intern* '%parse name))
        (write (intern* '%write name)))
    `(progn
       (define-parsed-struct ,name sf3-file
         ,@header)

       (defmethod id ((type ,name))
         ,id)

       (defmethod parse-for ((id (eql ,id)) io)
         (,parse io))

       (defmethod write-for ((,name ,name) io)
         (,write io ,name)))))
