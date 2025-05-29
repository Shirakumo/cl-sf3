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

(bs:define-io-structure line
  (color color)
  (thickness float32)
  (points (vector point uint16)))

(define-print-method line "~f ~d" thickness (length (line-points object)))

(bs:define-io-structure filled-shape
  (fill-color color)
  (outline-color color)
  (outline-thickness float32))

(bs:define-io-structure rectangle
  (:include filled-shape)
  (location point)
  (size size))

(define-print-method rectangle "~a ~a" location size)

(bs:define-io-structure circle
  (:include filled-shape)
  (location point)
  (size size))

(define-print-method circle "~a ~a" location size)

(bs:define-io-structure polygon
  (:include filled-shape)
  (points (vector point uint16)))

(define-print-method polygon "~d" (length (polygon-points object)))

(bs:define-io-structure curve
  (:include filled-shape)
  (points (vector point uint16)))

(define-print-method curve "~d" (length (curve-points object)))

(bs:define-io-structure text-shape
  (color color)
  (location point)
  (font (string uint16))
  (font-size float32)
  (text (string uint16)))

(define-print-method text-shape "~a ~a ~,2fun" location font font-size)

(bs:define-io-structure vector-graphic
  (width uint32)
  (height uint32)
  (count uint32)
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
                        (bs:slot count))))

(define-print-method vector-graphic "~dx~d ~d" width height (vector-graphic-count object))

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
