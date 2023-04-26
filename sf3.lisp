#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(bs:define-io-structure sf3-file-header
  #(#x81 #x53 #x46 #x33 #x00 #xE0 #xD0 #x0D #x0A #x0A)
  (type (case uint8
          (1 'archive)
          (2 'audio)
          (3 'image)
          (4 'log)
          (5 'model)
          (6 'text)
          (7 'vector-graphic))))

(bs:define-io-structure sf3-file
  (:include sf3-file-header)
  (content (case (bs:slot type)
             (archive archive)
             (audio audio)
             (image image)
             (log log)
             (model model)
             (text text)
             (vector-graphic vector-graphic))))

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
