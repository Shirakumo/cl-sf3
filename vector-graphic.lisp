(in-package #:org.shirakumo.sf3)

(bs:define-io-structure (color (:constructor %make-color))
  (r float32)
  (g float32)
  (b float32)
  (a float32))

(defun make-color (r g b &optional (a 1.0))
  (%make-color :r (float r 0f0)
               :g (float g 0f0)
               :b (float b 0f0)
               :a (float a 0f0)))

(define-print-method color "~,2f ~,2f ~,2f ~,2f" r g b a)

(bs:define-io-structure (point (:constructor %make-point))
  (x float32)
  (y float32))

(defun make-point (x y)
  (%make-point :x (float x 0f0)
               :y (float y 0f0)))

(define-print-method point "~,2f ~,2f" x y)

(bs:define-io-structure (size (:constructor %make-size))
  (w float32)
  (h float32))

(defun make-size (w h)
  (%make-size :w (float w 0f0)
              :h (float h 0f0)))

(define-print-method size "~,2f ~,2f" w h)

(bs:define-io-structure (shape-fill (:constructor %make-shape-fill))
  (fill-color color)
  (outline-color color)
  (outline-thickness float32))

(defun make-shape-fill (&key (fill-color (make-color 0 0 0))
                             (outline-color (make-color 0 0 0))
                             (outline-thickness 0))
  (%make-shape-fill :fill-color fill-color
                    :outline-color outline-color
                    :outline-thickness (float outline-thickness 0f0)))

(bs:define-io-structure (shape-bounds (:constructor %make-shape-bounds))
  (point point)
  (size size))

(defun make-shape-bounds (x y w h)
  (%make-shape-bounds :point (make-point x y) :size (make-size w h)))

(bs:define-io-structure (line (:constructor %make-line))
  (points (vector point uint16))
  (color color)
  (thickness float32))

(defun make-line (points &key (thickness 1) (color (make-color 0 0 0)))
  (%make-line :points (coerce points 'vector)
              :color color
              :thickness (float thickness 0f0)))

(define-print-method line "~f ~d" thickness (length (line-points object)))

(bs:define-io-structure (rectangle (:constructor %make-rectangle))
  (bounds shape-bounds)
  (fill shape-fill))

(defun make-rectangle (x y w h &rest fill-args)
  (%make-rectangle :bounds (make-shape-bounds x y w h)
                   :fill (apply #'make-shape-fill fill-args)))

(define-print-method rectangle "~a ~a" location size)

(bs:define-io-structure (circle (:constructor %make-circle))
  (bounds shape-bounds)
  (fill shape-fill))

(defun make-circle (x y w h &rest fill-args)
  (%make-circle :bounds (make-shape-bounds x y w h)
                :fill (apply #'make-shape-fill fill-args)))

(define-print-method circle "~a ~a" location size)

(bs:define-io-structure (polygon (:constructor %make-polygon))
  (points (vector point uint16))
  (fill shape-fill))

(defun make-polygon (points &rest fill-args)
  (%make-polygon :points (coerce points 'vector)
                 :fill (apply #'make-shape-fill fill-args)))

(define-print-method polygon "~d" (length (polygon-points object)))

(bs:define-io-structure (curve (:constructor %make-curve))
  (points (vector point uint16))
  (fill shape-fill))

(defun make-curve (points &rest fill-args)
  (%make-curve :points (coerce points 'vector)
               :fill (apply #'make-shape-fill fill-args)))

(define-print-method curve "~d" (length (curve-points object)))

(bs:define-io-structure (text-shape (:constructor %make-text-shape))
  (point point)
  (color color)
  (font (string uint16))
  (font-size float32)
  (text (string uint16)))

(defun make-text-shape (x y text &key (color (make-color 0 0 0)) (font "sans-serif") (font-size 12))
  (%make-text-shape :point (make-point x y)
                    :color color
                    :font font
                    :font-size (float font-size 0f0)
                    :text text))

(define-print-method text-shape "~a ~a ~,2fun" location font font-size)

(bs:define-io-structure (vector-graphic (:constructor %make-vector-graphic))
  (width uint32)
  (height uint32)
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
                        uint32)))

(defun make-vector-graphic (width height instructions)
  (%make-vector-graphic :width width :height height :instructions (coerce instructions 'vector)))

(define-print-method vector-graphic "~dx~d ~d" width height (length (vector-graphic-instructions object)))

(define-accessors color r g b a)
(define-accessors point x y)
(define-accessors size w h (width w) (height h))
(define-accessors shape-fill fill-color outline-color outline-thickness)
(define-accessors shape-bounds point size)
(define-accessors line points thickness color)
(define-accessors rectangle bounds fill)
(define-accessors circle bounds fill)
(define-accessors polygon points fill)
(define-accessors curve points fill)
(define-accessors text-shape point color font font-size text)
(define-accessors vector-graphic width height instructions)
