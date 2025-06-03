(in-package #:org.shirakumo.sf3)

(bs:define-io-structure (log-entry (:constructor %make-log-entry))
  (size uint32)
  (time uint64)
  (severity sint8)
  (source (string uint8))
  (category (string uint8))
  (message (string uint16)))

(defun make-log-entry (message &key (time 0)
                                    (severity 0)
                                    (source "")
                                    (category ""))
  (%make-log-entry :size (+ 4 8 1
                            1 1 (babel:string-size-in-octets source :encoding :utf-8)
                            1 1 (babel:string-size-in-octets category :encoding :utf-8)
                            2 1 (babel:string-size-in-octets message :encoding :utf-8))
                   :message message
                   :time time
                   :severity severity
                   :source source
                   :category category))

(define-print-method log-entry "~a ~d ~a ~a ~a"
  (format-time (log-entry-time object)) severity source category message)

(bs:define-io-structure (log-chunk (:constructor %make-log-chunk))
  (size uint64)
  (entry-count uint32)
  (entry-offsets (vector uint64 (bs:slot entry-count)))
  (entries (vector log-entry (bs:slot entry-count) (aref (bs:slot entry-offsets) bs:i))))

(defun make-log-chunk (&key (entries 1024))
  (let ((size (+ 8 4 (* 8 entries))))
    (%make-log-chunk :size size
                     :entry-count 0
                     :entry-offsets (make-array entries :element-type '(unsigned-byte 64) :initial-element size)
                     :entries (make-array entries :fill-pointer 0))))

(defun log-chunk-capacity (chunk)
  ;; NOTE: we use the first offset, which will point us right after the ENTRY-OFFSETS
  ;;       then we can simply subtract the SIZE and ENTRY-COUNT, and divide by the size
  ;;       to get the total capacity. The first offset is guaranteed to always be set.
  (/ (- (aref (log-chunk-entry-offsets chunk) 0) 8 4) 8))

(defun log-chunk-remaining (chunk)
  (- (log-chunk-capacity chunk)
     (log-chunk-entry-count chunk)))

(defun log-chunk-append-entry (chunk entry)
  (let ((capacity (log-chunk-capacity chunk))
        (index (log-chunk-entry-count chunk)))
    (cond ((= index capacity)
           (error "No more entries available in last log chunk."))
          ((< index (1- capacity))
           (setf (aref (log-chunk-entry-offsets chunk) (1+ index))
                 (+ (aref (log-chunk-entry-offsets chunk) index)
                    (log-entry-size entry)))))
    (setf (aref (log-chunk-entries chunk) index) entry)
    (setf (fill-pointer (log-chunk-entries chunk)) (1+ index))
    (incf (log-chunk-entry-count chunk))
    (incf (log-chunk-size chunk) (log-entry-size entry))
    chunk))

(define-print-method log-chunk "~d" entry-count)

(bs:define-io-structure (log (:constructor %make-log))
  (start-time sint64)
  (end-time sint64)
  (chunks (vector log-chunk uint16)))

(defun make-log (&key (start-time (get-universal-time)) end-time (entries 1024))
  (log-append-chunk (%make-log :start-time (universal-to-unix-time start-time)
                               :end-time (if end-time (universal-to-unix-time end-time) (1- (ash 1 63)))
                               :chunks (make-array 0 :adjustable T :fill-pointer T))
                    :entries entries))

(defun log-append-chunk (log &rest args)
  (vector-push-extend (apply #'make-log-chunk args) (log-chunks log))
  log)

(defun log-append-entry-extend (log entry &rest args)
  (let ((chunk (aref (log-chunks log) (1- (length (log-chunks log))))))
    (when (= 0 (log-chunk-remaining chunk))
      (setf chunk (apply #'make-log-chunk args))
      (vector-push-extend chunk (log-chunks log)))
    (log-chunk-append-entry chunk entry)
    log))

(defun log (log message &rest entry-args &key time &allow-other-keys)
  (if time
      (log-append-entry-extend log (apply #'make-log-entry message entry-args))
      (multiple-value-bind (time subsecs) (org.shirakumo.precise-time:get-precise-time)
        (let ((secs (- (universal-to-unix-time time) (log-start-time log)))
              (millis (truncate (* subsecs (/ 1000 ORG.SHIRAKUMO.PRECISE-TIME:PRECISE-TIME-UNITS-PER-SECOND)))))
          (log-append-entry-extend log (apply #'make-log-entry message :time (+ millis (* 1000 secs)) entry-args))))))

(define-print-method log "~a ~d" (format-time (log-start-time object)) chunk-count)

(define-accessors log-entry size time severity source category message)
(define-accessors log-chunk size entries)
(define-accessors log start-time end-time chunks)
