#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(bs:define-io-structure model
  (format uint8)
  (material-type uint8)
  (material-size uint32)
  (textures (vector (string uint16) (ldb (byte 4 0) (bs:slot material-type))))
  (faces (vector uint32 uint32) :offset (+ (bs:slot material-size) 6))
  (vertices (vector float32 uint32)))

(defun vertex-attributes (model)
  (ecase (model-format model)
    (#x03 '(:position))
    (#x05 '(:position :uv))
    (#x06 '(:position :color))
    (#x16 '(:position :normal))
    (#x08 '(:position :uv :normal))
    (#x09 '(:position :color :normal))
    (#x0B '(:position :uv :normal :tangent))
    (#x0C '(:position :color :normal :tangent))))

(defun texture-types (model)
  (ecase (model-material-type model)
    (#x00 '())
    (#x01 '(:albedo))
    (#x02 '(:albedo :normal))
    (#x12 '(:albedo :emission))
    (#x03 '(:albedo :normal :specular))
    (#x13 '(:albedo :normal :emission))
    (#x23 '(:albedo :normal :metallic))
    (#x04 '(:albedo :normal :metalness :roughness))
    (#x14 '(:albedo :normal :specular :emission))
    (#x24 '(:albedo :normal :metallic :emission))
    (#x05 '(:albedo :normal :metalness :roughness :emission))
    (#x15 '(:albedo :normal :metalness :roughness :occlusion))
    (#x06 '(:albedo :normal :metalness :roughness :occlusion :emission))))

(define-print-method model "~a ~d" (vertex-attributes object) (length (model-vertices object)))

(define-accessors model textures faces vertices)
