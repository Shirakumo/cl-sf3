(in-package #:org.shirakumo.sf3)

(bs:define-io-structure (sf3-file-header (:constructor %make-sf3-file-header))
  #(#x81 #x53 #x46 #x33 #x00 #xE0 #xD0 #x0D #x0A #x0A)
  (kind (case uint8
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

(defun make-sf3-file-header (kind &optional (checksum 0))
  (%make-sf3-file-header :kind kind :checksum checksum))

(define-print-method sf3-file-header "~a ~4,'0x" kind checksum)

(define-accessors sf3-file-header kind checksum)

(defgeneric file-extension (object)
  (:method ((_ bs::io-structure-object)) (file-extension (type-of _)))
  (:method ((_ (eql 'archive))) "ar.sf3")
  (:method ((_ (eql 'audio))) "au.sf3")
  (:method ((_ (eql 'image))) "img.sf3")
  (:method ((_ (eql 'log))) "log.sf3")
  (:method ((_ (eql 'model))) "mod.sf3")
  (:method ((_ (eql 'physics-model))) "phys.sf3")
  (:method ((_ (eql 'table))) "tab.sf3")
  (:method ((_ (eql 'text))) "txt.sf3")
  (:method ((_ (eql 'vector-graphic))) "vec.sf3"))

(defgeneric mime-type (object)
  (:method ((_ bs::io-structure-object)) (mime-type (type-of _)))
  (:method ((_ (eql 'archive))) "application/x.sf3-archive")
  (:method ((_ (eql 'audio))) "audio/x.sf3")
  (:method ((_ (eql 'image))) "image/x.sf3")
  (:method ((_ (eql 'log))) "application/x.sf3-log")
  (:method ((_ (eql 'model))) "model/x.sf3")
  (:method ((_ (eql 'physics-model))) "model/x.sf3-physics")
  (:method ((_ (eql 'table))) "application/x.sf3-table")
  (:method ((_ (eql 'text))) "application/x.sf3-text")
  (:method ((_ (eql 'vector-graphic))) "image/x.sf3-vector"))

(defun tell-sf3 (storage &rest args)
  (let ((values (multiple-value-list (apply #'read-sf3-file-header storage args))))
    (values-list (list* (sf3-file-header-kind (first values)) (rest values)))))

(defun read-sf3 (storage &rest args)
  (if (pathnamep storage)
      (with-open-file (stream storage :element-type '(unsigned-byte 8)
                                      :direction :input)
        (read-sf3 stream))
      (multiple-value-bind (header state) (apply #'read-sf3-file-header storage args)
        (etypecase storage
          (stream)
          (vector
           (setf (getf args :start) state))
          #+cffi
          (cffi:foreign-pointer
           (setf storage state)
           (decf (car args) (bs:octet-size header))))
        (ecase (sf3-file-header-kind header)
          (archive (apply #'read-archive storage args))
          (audio (apply #'read-audio storage args))
          (image (apply #'read-image storage args))
          (log (apply #'read-log storage args))
          (model (apply #'read-model storage args))
          (physics-model (apply #'read-physics-model storage args))
          (table (apply #'read-table storage args))
          (text (apply #'read-text storage args))
          (vector-graphic (apply #'read-vector-graphic storage args))))))

(defun write-sf3 (object storage &rest args)
  (etypecase storage
    (pathname
     (with-open-file (stream storage :element-type '(unsigned-byte 8)
                                     :direction :io
                                     :if-exists (getf args :if-exists :error))
       (write-sf3 object stream)))
    ((eql vector)
     (let ((array (make-array (+ 16 (bs:octet-size object)) :element-type '(unsigned-byte 8))))
       (apply #'write-sf3 object array args)
       array))
    (T
     (let ((header (make-sf3-file-header (type-of object))))
       (declare (dynamic-extent header))
       (let ((start-state (apply #'write-sf3-file-header header storage args)))
         (etypecase storage
           (stream
            (setf start-state (file-position storage)))
           (vector
            (setf (getf args :start) start-state))
           #+cffi
           (cffi:foreign-pointer
            (setf storage start-state)
            (decf (car args) (bs:octet-size header))))
         (let ((end-state (ecase (sf3-file-header-kind header)
                            (archive (apply #'write-archive object storage args))
                            (audio (apply #'write-audio object storage args))
                            (image (apply #'write-image object storage args))
                            (log (apply #'write-log object storage args))
                            (model (apply #'write-model object storage args))
                            (physics-model (apply #'write-physics-model object storage args))
                            (table (apply #'write-table object storage args))
                            (text (apply #'write-text object storage args))
                            (vector-graphic (apply #'write-vector-graphic object storage args)))))
           (etypecase storage
             (stream
              (cond ((not (input-stream-p storage))
                     (warn "Can't backfill CRC32 since the stream is not an input stream."))
                    (T
                     (finish-output storage)
                     (let* ((start start-state)
                            (end (file-position storage))
                            (crc (crc32 storage start end)))
                       (file-position storage (- start 5))
                       (setf (bst:uint32/io-stream storage) crc)))))
             (vector
              (let ((start (getf args :start))
                    (end end-state))
                (setf (bst:uint32/io-octet-vector storage (- start 5) start)
                      (crc32 storage start end))))
             #+cffi
             (cffi:foreign-pointer
              (let ((size (- (cffi:pointer-address end-state) (cffi:pointer-address storage))))
                (setf (bst:uint32/io-foreign-pointer (cffi:inc-pointer storage -5) 4)
                      (crc32 storage 0 size)))))))
       storage))))
