(in-package #:org.shirakumo.sf3)

(bs:define-io-structure color
  (r float32)
  (g float32)
  (b float32)
  (a float32))

(define-print-method color "~,2f ~,2f ~,2f ~,2f" r g b a)

(bs:define-io-structure point
  (x float32)
  (y float32))

(define-print-method point "~,2f ~,2f" x y)

(bs:define-io-structure size
  (w float32)
  (h float32))

(define-print-method size "~,2f ~,2f" w h)

(bs:define-io-structure shape-fill
  (fill-color color)
  (outline-color color)
  (outline-thickness float32))

(bs:define-io-structure shape-bounds
  (point point)
  (size size))

(bs:define-io-structure line
  (points (vector point uint16))
  (color color)
  (thickness float32))

(define-print-method line "~f ~d" thickness (length (line-points object)))

(bs:define-io-structure rectangle
  (bounds shape-bounds)
  (fill shape-fill))

(define-print-method rectangle "~a ~a" location size)

(bs:define-io-structure circle
  (bounds shape-bounds)
  (fill shape-fill))

(define-print-method circle "~a ~a" location size)

(bs:define-io-structure polygon
  (points (vector point uint16))
  (fill shape-fill))

(define-print-method polygon "~d" (length (polygon-points object)))

(bs:define-io-structure curve
  (points (vector point uint16))
  (fill shape-fill))

(define-print-method curve "~d" (length (curve-points object)))

(bs:define-io-structure text-shape
  (point point)
  (color color)
  (font (string uint16))
  (font-size float32)
  (text (string uint16)))

(define-print-method text-shape "~a ~a ~,2fun" location font font-size)

(bs:define-io-structure vector-graphic
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

(define-print-method vector-graphic "~dx~d ~d" width height (vector-graphic-count object))

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
