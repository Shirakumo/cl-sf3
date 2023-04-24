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

(define-accessors archive-meta-entry modification-time checksum mime-type path)
(define-accessors archive meta-entries files)
(define-accessors audio samplerate channels sample-format samples)
(define-accessors image width height depth channels data)
(define-accessors log-entry size time severity source category message)
(define-accessors log-chunk size entries)
(define-accessors log start-time chunks)
(define-accessors model textures faces vertices)
(define-accessors color-option color)
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
