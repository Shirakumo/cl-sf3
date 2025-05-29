(in-package #:org.shirakumo.sf3)

(bs:define-io-structure ellipsoid
  (width float32)
  (height float32)
  (depth float32))

(define-print-method ellipsoid "~fx~fx~f" width height depth)

(bs:define-io-structure box
  (width float32)
  (height float32)
  (depth float32))

(define-print-method box "~fx~fx~f" width height depth)

(bs:define-io-structure cylinder
  (bottom-radius float32)
  (top-radius float32)
  (height float32))

(define-print-method cylinder "~fx~fx~f" bottom-radius top-radius height)

(bs:define-io-structure pill
  (bottom-radius float32)
  (top-radius float32)
  (height float32))

(define-print-method pill "~fx~fx~f" bottom-radius top-radius height)

(bs:define-io-structure mesh
  (vertices (vector float32 (* uint16 3))))

(define-print-method mesh "~d" (truncate (length vertices) 3))

(bs:define-io-structure shape
  (transform (vector float32 16))
  (data (case uint8
          (#x01 ellipsoid)
          (#x02 box)
          (#x03 cylinder)
          (#x04 pill)
          (#x05 mesh))))

(define-print-method shape "~a" (type-of data))

(bs:define-io-structure physics-model
  (shape-count uint16)
  (mass float32)
  (tensor (vector float32 9))
  (shapes (vector shape (bs:slot shape-count))))

(define-print-method physics-model "~fkg ~d shapes" mass shape-count)

(define-accessors ellipsoid width height depth)
(define-accessors box width height depth)
(define-accessors cylinder bottom-radius top-radius height)
(define-accessors pill bottom-radius top-radius height)
(define-accessors mesh vertices)
(define-accessors shape transform data)
(define-accessors physics-model mass tensor shapes)

