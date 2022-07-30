#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(define-file-type model #x03
  (format (map :uint8
               (#x01 '(:position))
               (#x02 '(:position :uv))
               (#x12 '(:position :color))
               (#x22 '(:position :normal))
               (#x03 '(:position :uv :normal))
               (#x13 '(:position :color :normal))
               (#x04 '(:position :uv :normal :tangent))))
  (material-type (map :uint8
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
  (material-size :uint32)
  (count :uint32)
  (textures (vector (length material-type) (string :uint16)))
  (payload :payload))

(defun model-face-data (model)
  (let ((io (io model))
        (size (* (model-count model) 4)))
    (etypecase io
      (pointer-io
       (values (cffi:inc-pointer (pointer io) (model-payload model)) size))
      (vector-io
       (values (vector io) (model-payload model) (+ (model-payload model) size)))
      (file-stream
       (file-position io (model-payload model))
       (values io size)))))

(defun model-vertex-data (model)
  (let ((io (io model))
        (start (+ (model-payload model) (* (model-count model) 4))))
    (etypecase io
      (pointer-io
       (values (cffi:inc-pointer (pointer io) start) (- (end io) start)))
      (vector-io
       (values (vector io) start (end io)))
      (file-stream
       (file-position io start)
       (values io (- (file-length io) start))))))
