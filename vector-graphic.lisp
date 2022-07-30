#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(defstruct (instruction
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)))

(define-parsed-struct color NIL
  (r :float)
  (g :float)
  (b :float)
  (a :float))

(define-parsed-struct point NIL
  (x :float)
  (y :float))

(define-parsed-struct size NIL
  (w :float)
  (h :float))

(define-parsed-struct line instruction
  (color (object color))
  (thickness :float)
  (count :uint16)
  (edges (vector count (object point))))

(define-parsed-struct rectangle instruction
  (fill-color (object color))
  (outline-color (object color))
  (thickness :float)
  (position (object point))
  (size (object size)))

(define-parsed-struct circle instruction
  (fill-color (object color))
  (outline-color (object color))
  (thickness :float)
  (position (object point))
  (size (object size)))

(define-parsed-struct polygon instruction
  (fill-color (object color))
  (outline-color (object color))
  (thickness :float)
  (count :uint16)
  (edges (vector count (object point))))

(define-parsed-struct curve instruction
  (fill-color (object color))
  (outline-color (object color))
  (thickness :float)
  (count :uint16)
  (control-points (vector count (object point))))

(define-parsed-struct vector-text instruction
  (color (object color))
  (position (object point))
  (font (string :uint16))
  (font-size :float)
  (text (string :uint16)))

(define-parsed-struct matrix instruction
  (contents (vector 6 :float)))

(define-file-type vector-graphic #x06
  (width :uint32)
  (height :uint32)
  (count :uint32)
  (instructions (vector count (object instruction))))

(define-io-fun %parse-instruction (io)
  (ecase (ref :uint8)
    (#x01 (%parse-line io))
    (#x02 (%parse-rectangle io))
    (#x03 (%parse-circle io))
    (#x04 (%parse-polygon io))
    (#x05 (%parse-curve io))
    (#x06 (%parse-vector-text io))
    (#x10 (let ((matrix (%make-matrix io)))
            (setf (matrix-contents matrix)
                  (make-array 6 :element-type 'single-float :initial-contents '(1f0 0f0 0f0
                                                                                0f0 1f0 0f0)))
            matrix))
    (#x11 (%parse-matrix io))))

(define-io-fun %write-instruction (io instruction)
  (etypecase instruction
    (line
     (set :uint8 #x01)
     (%write-line io instruction))
    (rectangle
     (set :uint8 #x02)
     (%write-rectangle io instruction))
    (circle
     (set :uint8 #x03)
     (%write-circle io instruction))
    (polygon
     (set :uint8 #x04)
     (%write-polygon io instruction))
    (curve
     (set :uint8 #x05)
     (%write-curve io instruction))
    (vector-text
     (set :uint8 #x06)
     (%write-vector-text io instruction))
    (matrix
     (cond ((equal #(1f0 0f0 0f0 0f0 1f0 0f0) (matrix-contents instruction))
            (set :uint8 #x10))
           (T
            (set :uint8 #x11)
            (%write-matrix io instruction))))))
