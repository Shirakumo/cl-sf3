(in-package #:org.shirakumo.sf3)

(defmacro define-accessor (name type &optional (slot name))
  `(progn (defmethod ,name ((entry ,type))
            (,(intern (format NIL "~a-~a" type slot)) entry))
          (defmethod (setf ,name) (value (entry ,type))
            (setf (,(intern (format NIL "~a-~a" type slot)) entry) value))))

(defmacro define-accessors (type &rest names)
  `(progn ,@(loop for name in names 
                  for (n s) = (if (listp name) name (list name name))
                  collect `(define-accessor ,n ,type ,s))))

(defmacro define-delegates (type slot &rest delegates)
  `(progn ,@(loop for delegate in delegates
                  collect `(defmethod ,delegate ((entry ,type))
                             (,delegate (,(intern (format NIL "~a-~a" type slot)) entry)))
                  collect `(defmethod (setf ,delegate) (value (entry ,type))
                             (setf (,delegate (,(intern (format NIL "~a-~a" type slot)) entry)) value)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-crc32-table ()
    (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
      (loop for n below (length table)
            do (setf (aref table n)
                     (let ((c n))
                       (loop for k below 8
                             if (oddp c)
                             do (setf c (logxor #xedb88320 (ash c -1)))
                             else
                             do (setf c (ash c -1)))
                       c)))
      table)))

(declaim (type (simple-array (unsigned-byte 32) (256)) +crc32/table+))
(alexandria:define-constant +crc32/table+
    (generate-crc32-table) :test 'equalp)

(declaim (inline crc32-rotate))
(defun crc32-rotate (crc byte)
  (logxor (ldb (byte 24 8) crc) (aref +crc32/table+ (ldb (byte 8 0) (logxor crc byte)))))

(defun crc32 (file)
  (declare (optimize speed (safety 1)))
  (let ((crc #xffffffff))
    (declare (type (unsigned-byte 32) crc))
    (etypecase file
      ((or pathname string)
       (with-open-file (stream file :element-type '(unsigned-byte 8))
         (handler-case (loop (setf crc (crc32-rotate crc (read-byte stream))))
           (end-of-file ()))))
      ((vector (unsigned-byte 8))
       (loop for byte across file
             do (setf crc (crc32-rotate crc byte)))))
    (logxor crc #xFFFFFFFF)))

(declaim (inline universal-to-unix-time))
(defun universal-to-unix-time (universal-time)
  (- universal-time (encode-universal-time 0 0 0 1 1 1970 0)))

(declaim (inline unix-to-universal-time))
(defun unix-to-universal-time (unix-time)
  (+ unix-time (encode-universal-time 0 0 0 1 1 1970 0)))

(defun format-time (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time (+ timestamp (encode-universal-time 0 0 0 1 1 1970 0)))
    (format NIL "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s)))

(defmacro define-print-method (class format &rest format-args)
  `(defmethod print-object ((object ,class) stream)
     (print-unreadable-object (object stream :type T)
       (format stream ,format ,@(loop for arg in format-args
                                      collect (if (symbolp arg) `(slot-value object ',arg) arg))))))

(defun lisp-type->bs-type (lisp-type)
  (dolist (type (bs:list-io-types) (error "Don't know how to translate ~s to a binary-structures type."
                                          lisp-type))
    (when (and (equal (bs:lisp-type type) lisp-type)
               (if (typep (bs:io-type type) 'bs:numeric-type)
                   (eql :little-endian (bs:order (bs:io-type type)))
                   T))
      (return type))))

(defun set-equal (a b)
  (null (set-exclusive-or a b)))
