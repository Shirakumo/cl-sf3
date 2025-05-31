(in-package #:org.shirakumo.sf3)

(defvar *identity* (make-array 16 :element-type 'single-float :initial-contents
                               '(1f0 0f0 0f0 0f0
                                 0f0 1f0 0f0 0f0
                                 0f0 0f0 1f0 0f0
                                 0f0 0f0 0f0 1f0)))

(bs:define-io-structure (ellipsoid (:constructor %make-ellipsoid))
  (width float32)
  (height float32)
  (depth float32))

(defun make-ellipsoid (w h d &key transform)
  (make-shape :transform (if transform (coerce transform '(simple-array single-float (16))) *identity*)
              :data (%make-ellipsoid :width (float w 0f0)
                                     :height (float h 0f0)
                                     :depth (float d 0f0))))

(define-print-method ellipsoid "~fx~fx~f" width height depth)

(bs:define-io-structure (box (:constructor %make-box))
  (width float32)
  (height float32)
  (depth float32))

(defun make-box (w h d &key transform)
  (make-shape :transform (if transform (coerce transform '(simple-array single-float (16))) *identity*)
              :data (%make-box :width (float w 0f0)
                               :height (float h 0f0)
                               :depth (float d 0f0))))

(define-print-method box "~fx~fx~f" width height depth)

(bs:define-io-structure (cylinder (:constructor %make-cylinder))
  (bottom-radius float32)
  (top-radius float32)
  (height float32))

(defun make-cylinder (bottom-radius top-radius height &key transform)
  (make-shape :transform (if transform (coerce transform '(simple-array single-float (16))) *identity*)
              :data (%make-cylinder :bottom-radius (float bottom-radius 0f0)
                                    :top-radius (float top-radius 0f0)
                                    :height (float height 0f0))))

(define-print-method cylinder "~fx~fx~f" bottom-radius top-radius height)

(bs:define-io-structure (pill (:constructor %make-pill))
  (bottom-radius float32)
  (top-radius float32)
  (height float32))

(defun make-pill (bottom-radius top-radius height &key transform)
  (make-shape :transform (if transform (coerce transform '(simple-array single-float (16))) *identity*)
              :data (%make-pill :bottom-radius (float bottom-radius 0f0)
                                :top-radius (float top-radius 0f0)
                                :height (float height 0f0))))

(define-print-method pill "~fx~fx~f" bottom-radius top-radius height)

(bs:define-io-structure (mesh (:constructor %make-mesh))
  (vertex-count uint16)
  (vertices (vector float32 (* (bs:slot vertex-count) 3))))

(defun make-mesh (vertices &key transform)
  (make-shape :transform (if transform (coerce transform '(simple-array single-float (16))) *identity*)
              :data (%make-mesh :vertex-count (truncate (length vertices) 3)
                                :vertices (coerce vertices '(simple-array single-float (*))))))

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

(bs:define-io-structure (physics-model (:constructor %make-physics-model))
  (mass float32)
  (tensor (vector float32 9))
  (shapes (vector shape uint16)))

(defun make-physics-model (mass tensor &rest shapes)
  (%make-physics-model :mass (float mass 0f0)
                       :tensor (coerce tensor '(simple-array single-float (9)))
                       :shapes (coerce shapes 'vector)))

(define-print-method physics-model "~fkg ~d shapes" mass shape-count)

(define-accessors ellipsoid width height depth)
(define-accessors box width height depth)
(define-accessors cylinder bottom-radius top-radius height)
(define-accessors pill bottom-radius top-radius height)
(define-accessors mesh vertices)
(define-accessors shape transform data)
(define-accessors physics-model mass tensor shapes)

