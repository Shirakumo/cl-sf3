(in-package #:org.shirakumo.sf3)

(bs:define-io-structure column-spec
  (name (string uint16))
  (length uint32)
  (kind uint8))

(define-print-method column-spec "~s ~d ~a" name length type)

(defun column-spec-type (spec)
  (ecase (column-spec-kind spec)
    (#x01 'uint8)
    (#x02 'uint16)
    (#x04 'uint32)
    (#x08 'uint64)
    (#x11 'sint8)
    (#x12 'sint16)
    (#x14 'sint32)
    (#x18 'sint64)
    (#x22 'float16)
    (#x24 'float32)
    (#x28 'float64)
    (#x48 'timestamp)
    (#x58 'high-resolution-timestamp)
    (#x61 'boolean)))

(defun column-spec-element-size (spec)
  (ldb (byte 4 0) (column-spec-kind spec)))

(defun column-spec-element-count (spec)
  (/ (column-spec-length spec) (column-spec-element-size spec)))

(bs:define-io-structure table
  (spec-length uint32)
  (column-count uint16)
  (row-length uint64)
  (row-count uint64)
  (column-specs (vector column-spec (bs:slot column-count)))
  (row (vector (vector uint8 (bs:slot row-length)) (bs:slot row-count))))

(define-print-method table "~dx~d" column-count row-count)
