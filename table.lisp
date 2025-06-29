(in-package #:org.shirakumo.sf3)

(bs:define-io-structure (column-spec (:constructor %make-column-spec))
  (length uint32)
  (kind uint8)
  (name (string uint16)))

(defun make-column-spec (name &optional (type :string) length)
  (%make-column-spec :name name
                     :length (case type
                               (:string
                                (or length 64))
                               (:timestamp
                                (* (or length 1) (bs:octet-size :unix-time-u64)))
                               (:high-resolution-timestamp
                                (* (or length 1) (bs:octet-size 'high-resolution-timestamp)))
                               (:boolean
                                (* (or length 1) (bs:octet-size 'boolean)))
                               (T
                                (* (or length 1) (bs:octet-size type))))
                     :kind (type->column-spec-kind type)))

(define-print-method column-spec "~s ~d ~a" name length kind)

(define-accessors column-spec name (size length) kind)

(defun column-spec-type (spec)
  (ecase (column-spec-kind spec)
    (#x01 :uint8)
    (#x02 :uint16)
    (#x04 :uint32)
    (#x08 :uint64)
    (#x11 :sint8)
    (#x12 :sint16)
    (#x14 :sint32)
    (#x18 :sint64)
    (#x22 :float16)
    (#x24 :float32)
    (#x28 :float64)
    (#x31 :string)
    (#x48 :timestamp)
    (#x58 :high-resolution-timestamp)
    (#x61 :boolean)))

(defun type->column-spec-kind (type)
  (case type
    (:uint8 #x01)
    (:uint16 #x02)
    (:uint32 #x04)
    (:uint64 #x08)
    (:sint8 #x11)
    (:sint16 #x12)
    (:sint32 #x14)
    (:sint64 #x18)
    (:float16 #x22)
    (:float32 #x24)
    (:float64 #x28)
    (:string #x31)
    (:timestamp #x48)
    (:high-resolution-timestamp #x58)
    (:boolean #x61)))

(bs:define-io-type (bs:io-timestamp high-resolution-timestamp)
  :epoch :unix :octet-size 8 :resolution 1000000000)

(macrolet ((define-octet-accessor (backend io-type)
             (let ((type (bs:io-type io-type)))
               `(progn
                  ,(bs:read-defun backend type io-type)
                  ,(bs:write-defun backend type `(setf ,io-type))))))
  (define-octet-accessor bs:io-octet-vector high-resolution-timestamp))

(defun column-spec-reader (spec)
  (ecase (column-spec-kind spec)
    (#x01 #'bst:uint8/io-octet-vector)
    (#x02 #'bst:uint16/io-octet-vector)
    (#x04 #'bst:uint32/io-octet-vector)
    (#x08 #'bst:uint64/io-octet-vector)
    (#x11 #'bst:sint8/io-octet-vector)
    (#x12 #'bst:sint16/io-octet-vector)
    (#x14 #'bst:sint32/io-octet-vector)
    (#x18 #'bst:sint64/io-octet-vector)
    (#x22 #'bst:uint16/io-octet-vector)
    (#x24 #'bst:float32/io-octet-vector)
    (#x28 #'bst:float64/io-octet-vector)
    (#x31 #'bst:utf8-string/io-octet-vector)
    (#x48 #'bst:unix-time-s64/io-octet-vector)
    (#x58 #'high-resolution-timestamp)
    (#x61 #'bst:boolean/io-octet-vector)))

(defun column-spec-writer (spec)
  (ecase (column-spec-kind spec)
    (#x01 #'(setf bst:uint8/io-octet-vector))
    (#x02 #'(setf bst:uint16/io-octet-vector))
    (#x04 #'(setf bst:uint32/io-octet-vector))
    (#x08 #'(setf bst:uint64/io-octet-vector))
    (#x11 #'(setf bst:sint8/io-octet-vector))
    (#x12 #'(setf bst:sint16/io-octet-vector))
    (#x14 #'(setf bst:sint32/io-octet-vector))
    (#x18 #'(setf bst:sint64/io-octet-vector))
    (#x22 #'(setf bst:uint16/io-octet-vector))
    (#x24 #'(setf bst:float32/io-octet-vector))
    (#x28 #'(setf bst:float64/io-octet-vector))
    (#x31 #'(setf bst:utf8-string/io-octet-vector))
    (#x48 #'(setf bst:unix-time-s64/io-octet-vector))
    (#x58 #'(setf high-resolution-timestamp))
    (#x61 #'(setf bst:boolean/io-octet-vector))))

(defun column-spec-element-size (spec)
  (ldb (byte 4 0) (column-spec-kind spec)))

(defun column-spec-element-count (spec)
  (if (= #x31 (column-spec-kind spec))
      1
      (/ (column-spec-length spec) (column-spec-element-size spec))))

(bs:define-io-structure (table (:constructor %make-table))
  (column-count uint16)
  (row-length uint64)
  (row-count uint64)
  (spec-length uint32)
  (column-specs (vector column-spec (bs:slot column-count)))
  (row-data (vector uint8 (* (bs:slot row-length) (bs:slot row-count)))))

(defun make-table (columns data)
  (let* ((columns (coerce (loop for spec in columns
                                collect (etypecase spec
                                          (list (apply #'make-column-spec spec))
                                          (string (make-column-spec spec))
                                          (column-spec spec)))
                          'vector))
         (column-count (length columns))
         (row-count (length data))
         (row-length (reduce #'+ columns :key #'column-spec-length))
         (table (%make-table :column-count column-count
                             :row-length row-length
                             :row-count row-count
                             :spec-length (reduce #'+ columns :key #'bs:octet-size)
                             :column-specs columns
                             :row-data (make-array (* row-count row-length) :element-type '(unsigned-byte 8)))))
    (dotimes (r row-count table)
      (let ((row (elt data r)))
        (dotimes (c column-count)
          (setf (cell table r c) (elt row c)))))))

(define-print-method table "~dx~d" column-count row-count)

(define-accessors table column-count row-count column-specs row-data)

(defun row (table row)
  (loop for column from 0 below (table-column-count table)
        collect (cell table row column)))

(defun cell (table row column)
  (let* ((data (table-row-data table))
         (specs (table-column-specs table))
         (spec (aref specs column))
         (row-start (* row (table-row-length table)))
         (cell-start (+ row-start (loop for i from 0 below column
                                        sum (column-spec-length (aref specs i)))))
         (cell-end (+ cell-start (column-spec-length spec)))
         (reader (column-spec-reader spec))
         (i cell-start))
    (loop while (< i cell-end)
          collect (multiple-value-bind (v next) (funcall reader data i cell-end)
                    (if (and (= cell-start i) (= cell-end next))
                        (return v)
                        (setf i next))
                    v))))

(defun (setf cell) (value table row column)
  (let* ((data (table-row-data table))
         (specs (table-column-specs table))
         (spec (aref specs column))
         (row-start (* row (table-row-length table)))
         (cell-start (+ row-start (loop for i from 0 below column
                                        sum (column-spec-length (aref specs i)))))
         (cell-end (+ cell-start (column-spec-length spec)))
         (writer (column-spec-writer spec))
         (i cell-start))
    (if (listp value)
        (loop while (and (< i cell-end) value)
              collect (setf i (funcall writer (pop value) data i cell-end)))
        (funcall writer value data cell-start cell-end))
    value))
