(in-package #:org.shirakumo.sf3)

(bs:define-io-structure log-entry
  (size uint32)
  (time uint64)
  (severity int8)
  (source (string uint8))
  (category (string uint8))
  (message (string uint16)))

(define-print-method log-entry "~a ~d ~a ~a ~a"
  (format-time (log-entry-time object)) severity source category message)

(bs:define-io-structure log-chunk
  (size uint64)
  (entry-count uint32)
  (entry-offsets (vector uint64 (bs:slot entry-count)))
  (entries (vector log-entry (bs:slot entry-count) (aref (bs:slot entry-offsets) bs:i))))

(define-print-method log-chunk "~d" entry-count)

(bs:define-io-structure log
  (start-time int64)
  (chunk-count uint16)
  (chunks (vector log-chunk (bs:slot chunk-count))))

(define-print-method log "~a ~d" (format-time (log-start-time object)) chunk-count)

(define-accessors log-entry size time severity source category message)
(define-accessors log-chunk size entries)
(define-accessors log start-time chunks)
