#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(defmacro define-accessor (name type &optional (slot name))
  `(progn (defmethod ,name ((entry ,type))
            (,(intern (format NIL "~a-~a" type slot)) entry))
          (defmethod (setf ,name) (value (entry ,type))
            (setf (,(intern (format NIL "~a-~a" type slot)) entry) value))))

(defmacro define-accessors (type &rest names)
  `(progn ,@(loop for name in names 
                  for (n s) = (if (listp name) name (list name name))
                  collect `(define-accessor ,n ,type ,s))))

(defmacro define-delegates (type slot &rest delegates)
  `(progn ,@(loop for delegate in delegates
                  collect `(defmethod ,delegate ((entry ,type))
                             (,delegate (,(intern (format NIL "~a-~a" type slot)) entry)))
                  collect `(defmethod (setf ,delegate) (value (entry ,type))
                             (setf (,delegate (,(intern (format NIL "~a-~a" type slot)) entry)) value)))))

(define-accessors archive-meta-entry modification-time checksum mime-type path)
(define-accessors archive meta-entries files)
(define-accessors audio samplerate channels sample-format samples)
(define-accessors image width height depth channels data)
(define-accessors log-entry size time severity source category message)
(define-accessors log-chunk size entries)
(define-accessors log start-time chunks)
(define-accessors model textures faces vertices)
(define-accessors color-option r g b)
(define-accessors size-option size)
(define-accessors heading-option level)
(define-accessors link-option address)
(define-accessors target-option address)
(define-accessors text markup text)
(define-accessors color r g b a)
(define-accessors point x y)
(define-accessors size w h (width w) (height h))
(define-accessors line color thickness points)
(define-accessors filled-shape fill-color outline-color outline-thickness)
(define-accessors rectangle location size)
(define-accessors circle location size)
(define-accessors polygon points)
(define-accessors curve points)
(define-accessors text-shape color location font font-size text)
(define-accessors vector-graphic width height instructions)
(define-accessors sf3-file content)

(define-delegates sf3-file content modification-time checksum mime-type path meta-entries files samplerate channels sample-format samples width height depth data size time severity source category message entries start-time chunks textures faces vertices color level address markup text r g b a x y w h thickness points fill-color outline-color outline-thickness location font font-size instructions)

(defgeneric file-extension (object)
  (:method ((_ sf3-file)) (mime-type (sf3-file-content _)))
  (:method ((_ archive)) "ar.sf3")
  (:method ((_ audio)) "au.sf3")
  (:method ((_ image)) "img.sf3")
  (:method ((_ log)) "log.sf3")
  (:method ((_ model)) "mod.sf3")
  (:method ((_ text)) "txt.sf3")
  (:method ((_ vector-graphic)) "vec.sf3"))

(defgeneric mime-type (object)
  (:method ((_ sf3-file)) (mime-type (sf3-file-content _)))
  (:method ((_ archive)) "application/x.sf3-archive")
  (:method ((_ audio)) "audio/x.sf3")
  (:method ((_ image)) "image/x.sf3")
  (:method ((_ log)) "application/x.sf3-log")
  (:method ((_ model)) "model/x.sf3")
  (:method ((_ text)) "text/x.sf3")
  (:method ((_ vector-graphic)) "image/x.sf3-vector"))

(defun read-sf3 (storage &rest args)
  (sf3-file-content (apply #'read-sf3-file storage args)))

(defun write-sf3 (object storage &rest args)
  (let ((sf3-file (make-sf3-file :content object)))
    (declare (dynamic-extent sf3-file))
    (apply #'write-sf3-file storage args)))

(defun tell-sf3 (storage &rest args)
  (let ((values (multiple-value-list (apply #'read-sf3-file-header storage args))))
    (values-list (list* (sf3-file-header-type (first values)) (rest values)))))
