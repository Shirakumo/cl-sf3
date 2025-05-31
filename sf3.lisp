(in-package #:org.shirakumo.sf3)

(bs:define-io-structure sf3-file-header
  #(#x81 #x53 #x46 #x33 #x00 #xE0 #xD0 #x0D #x0A #x0A)
  (type (case uint8
          (1 'archive)
          (2 'audio)
          (3 'image)
          (4 'log)
          (5 'model)
          (6 'physics-model)
          (7 'table)
          (8 'text)
          (9 'vector-graphic)))
  (checksum uint32)
  #(#x00))

(bs:define-io-structure sf3-file
  (:include sf3-file-header)
  (content (case (bs:slot type)
             (archive archive)
             (audio audio)
             (image image)
             (log log)
             (model model)
             (physics-model physics-model)
             (table table)
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
  (:method ((_ physics-model)) "phys.sf3")
  (:method ((_ table)) "tab.sf3")
  (:method ((_ text)) "txt.sf3")
  (:method ((_ vector-graphic)) "vec.sf3"))

(defgeneric mime-type (object)
  (:method ((_ sf3-file)) (mime-type (sf3-file-content _)))
  (:method ((_ archive)) "application/x.sf3-archive")
  (:method ((_ audio)) "audio/x.sf3")
  (:method ((_ image)) "image/x.sf3")
  (:method ((_ log)) "application/x.sf3-log")
  (:method ((_ model)) "model/x.sf3")
  (:method ((_ physics-model)) "model/x.sf3-physics")
  (:method ((_ table)) "application/x.sf3-table")
  (:method ((_ text)) "text/x.sf3")
  (:method ((_ vector-graphic)) "image/x.sf3-vector"))

(defun read-sf3 (storage &rest args)
  (sf3-file-content (apply #'read-sf3-file storage args)))

(defun write-sf3 (object storage &rest args)
  (let ((sf3-file (make-sf3-file
                   :checksum 0
                   :type (type-of object)
                   :content object)))
    (declare (dynamic-extent sf3-file))
    (apply #'write-sf3-file sf3-file storage args)))

(defun tell-sf3 (storage &rest args)
  (let ((values (multiple-value-list (apply #'read-sf3-file-header storage args))))
    (values-list (list* (sf3-file-header-type (first values)) (rest values)))))
