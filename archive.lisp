#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(bs:define-io-structure archive-meta-entry
  (modification-time uint64)
  (checksum uint32)
  (mime-type (string uint8))
  (path (string uint16)))

(define-print-method archive-meta-entry "~a ~a" mime-type path)

(bs:define-io-structure archive
  (count uint64)
  (meta-size uint64)
  (meta-offsets (vector uint64 (bs:slot count)))
  (meta-entries (vector archive-meta-entry (bs:slot count) (aref (bs:slot meta-offsets) bs:i)))
  (file-offsets (vector uint64 (bs:slot count)) :offset (+ 8 8 (bs:slot meta-size)))
  (files (vector (vector uint8 uint64) (bs:slot count) (aref (bs:slot file-offsets) bs:i))))

(define-print-method archive "~d" count)

(define-accessors archive-meta-entry modification-time checksum mime-type path)
(define-accessors archive meta-entries files)

(defun %add-file (file path mime archive)
  (let ((entry (make-archive-meta-entry :modification-time (file-write-date file)
                                        :checksum (crc32 file)
                                        :mime-type (or mime "application/octet-stream")
                                        :path path)))
    (vector-push-extend (archive-meta-size archive) (archive-meta-offsets archive))
    (vector-push-extend entry (archive-meta-entries archive))
    (incf (archive-meta-size archive) (bs:octet-size entry))
    (vector-push-extend (if (< 0 (archive-count archive))
                            (+ (aref (archive-file-offsets archive) (1- (archive-count archive)))
                               (length (aref (archive-files archive) (1- (archive-count archive)))))
                            0)
                        (archive-file-offsets archive))
    (vector-push-extend (alexandria:read-file-into-byte-vector file)
                        (archive-files archive))
    (incf (archive-count archive))))

(defmethod add-file ((file pathname) (archive archive) &key mime-type path)
  (labels ((add (file path mime)
             (cond ((wild-pathname-p file)
                    (dolist (sub (directory file))
                      (add sub (merge-pathnames (enough-namestring sub file) (or path #p"")) "application/octet-stream")))
                   ((pathname-utils:directory-p file)
                    (dolist (sub (org.shirakumo.filesystem-utils:list-contents file))
                      (add sub (merge-pathnames (enough-namestring sub file) (or path #p"")) "application/octet-stream")))
                   (T
                    (%add-file file (or path (file-namestring file)) mime-type archive)))))
    (add file path mime-type)))

(defmethod extract-file ((file integer) (archive archive) &key path (if-exists :error) (verify T))
  (let* ((meta (aref (archive-meta-entries archive) file))
         (path (or path (archive-meta-entry-path meta))))
    (ensure-directories-exist path)
    (alexandria:write-byte-vector-into-file (aref (archive-files archive) file) path :if-exists if-exists)
    (when verify (assert (= (archive-meta-entry-checksum meta) (crc32 path))))))

(defmethod extract-file ((all (eql T)) (archive archive) &key path (if-exists :error) (verify T))
  (loop with metas = (archive-meta-entries archive)
        with files = (archive-files archive)
        for i from 0 below (archive-count archive)
        for meta = (aref metas i)
        for bytes = (aref files i)
        for file-path = (merge-pathnames (archive-meta-entry-path meta) path)
        do (ensure-directories-exist file-path)
           (alexandria:write-byte-vector-into-file bytes file-path :if-exists if-exists)
           (when verify (assert (= (archive-meta-entry-checksum meta) (crc32 file-path))))))
