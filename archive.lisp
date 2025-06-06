(in-package #:org.shirakumo.sf3)

(bs:define-io-structure archive-meta-entry
  (modification-time bs:unix-time-s64)
  (checksum uint32)
  (mime-type (string uint8))
  (path (string uint16)))

(define-print-method archive-meta-entry "~a ~a" mime-type path)

(bs:define-io-structure (archive (:constructor %make-archive))
  (count uint64)
  (meta-size uint64)
  (meta-offsets (vector uint64 (bs:slot count)))
  (meta-entries (vector archive-meta-entry (bs:slot count) (aref (bs:slot meta-offsets) bs:i)))
  (file-offsets (vector uint64 (bs:slot count)) :offset (+ 8 8 (bs:slot meta-size)))
  (files (vector (vector uint8 uint64) (bs:slot count) (aref (bs:slot file-offsets) bs:i))))

(define-print-method archive "~d" count)

(define-accessors archive-meta-entry modification-time checksum mime-type path)
(define-accessors archive meta-entries files)

(defun make-archive (files)
  (let ((archive (%make-archive
                  :meta-offsets (make-array 0 :adjustable T :fill-pointer T :element-type '(unsigned-byte 64))
                  :meta-entries (make-array 0 :adjustable T :fill-pointer T)
                  :file-offsets (make-array 0 :adjustable T :fill-pointer T :element-type '(unsigned-byte 64))
                  :files (make-array 0 :adjustable T :fill-pointer T))))
    (loop for file in files
          do (if (listp file)
                 (apply #'add-file (car file) archive (cdr file))
                 (add-file file archive)))
    archive))

(defun %add-file (contents path mime modification-time archive)
  (let ((entry (make-archive-meta-entry :modification-time modification-time
                                        :checksum (crc32 contents)
                                        :mime-type (or mime "application/octet-stream")
                                        :path path)))
    (vector-push-extend (if (< 0 (archive-count archive))
                            (+ (aref (archive-meta-offsets archive) (1- (archive-count archive)))
                               (bs:octet-size entry))
                            0)
                        (archive-meta-offsets archive))
    (vector-push-extend entry (archive-meta-entries archive))
    (incf (archive-meta-size archive) (+ (bs:octet-size entry) 8))
    (vector-push-extend (if (< 0 (archive-count archive))
                            (+ (aref (archive-file-offsets archive) (1- (archive-count archive)))
                               8 (length (aref (archive-files archive) (1- (archive-count archive)))))
                            0)
                        (archive-file-offsets archive))
    (vector-push-extend contents (archive-files archive))
    (incf (archive-count archive))
    archive))

(defmethod add-file ((file vector) (archive archive) &key (mime-type "application/octet-stream") path (modification-time (get-universal-time)))
  (%add-file file
             (or path (error "PATH required"))
             (or mime-type (error "MIME-TYPE required"))
             modification-time
             archive))

(defmethod add-file ((file string) (archive archive) &key (mime-type "text/plain") path (modification-time (get-universal-time)))
  (%add-file (babel:string-to-octets file :encoding :utf-8)
             (or path (error "PATH required"))
             (or mime-type (error "MIME-TYPE required"))
             modification-time
             archive))

(defmethod add-file ((file pathname) (archive archive) &key mime-type path modification-time)
  (labels ((add (file path mime)
             (cond ((wild-pathname-p file)
                    (dolist (sub (directory file))
                      (add sub (merge-pathnames (enough-namestring sub file) (or path #p"")) mime)))
                   ((pathname-utils:directory-p file)
                    (dolist (sub (org.shirakumo.filesystem-utils:list-contents file))
                      (add sub (merge-pathnames (enough-namestring sub file) (or path #p"")) mime)))
                   (T
                    (%add-file (alexandria:read-file-into-byte-vector file)
                               (if path
                                   (org.shirakumo.pathname-utils:unix-namestring path)
                                   (file-namestring file))
                               mime
                               (or modification-time (file-write-date file)) archive)))))
    (add file path mime-type)))

(defmethod extract-file ((file integer) (archive archive) &key path (if-exists :error) (verify T) (preserve-modification-time T))
  (let* ((meta (aref (archive-meta-entries archive) file))
         (path (or path (archive-meta-entry-path meta))))
    (ensure-directories-exist path)
    (alexandria:write-byte-vector-into-file (aref (archive-files archive) file) path :if-exists if-exists)
    (when preserve-modification-time
      (setf (org.shirakumo.file-attributes:modification-time path)
            (archive-meta-entry-modification-time meta)))
    (when verify
      (assert (= (archive-meta-entry-checksum meta) (crc32 path))))
    path))

(defmethod extract-file ((file string) (archive archive) &rest args &key &allow-other-keys)
  (loop with metas = (archive-meta-entries archive)
        for i from 0 below (archive-count archive)
        for file-path = (archive-meta-entry-path (aref metas i))
        do (when (string= file file-path)
             (return (apply #'extract-file i archive args)))
        finally (error "No file with path~%  ~a~%in archive ~a" file archive)))

(defmethod extract-file ((all (eql T)) (archive archive) &key path (if-exists :error) (verify T) (preserve-modification-time T))
  (loop with metas = (archive-meta-entries archive)
        for i from 0 below (archive-count archive)
        for file-path = (merge-pathnames (archive-meta-entry-path (aref metas i)) path)
        do (extract-file i archive :preserve-modification-time preserve-modification-time
                                   :verify verify
                                   :if-exists if-exists
                                   :path file-path))
  path)
