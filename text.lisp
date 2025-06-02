(in-package #:org.shirakumo.sf3)

(bs:define-io-structure (color-option (:constructor %make-color-option))
  (r float32)
  (g float32)
  (b float32))

(defun make-color-option (start end &optional (r 0) (g 0) (b 0))
  (%make-color-option :r (float r 0f0)
                      :g (float g 0f0)
                      :b (float b 0f0)))

(define-print-method color-option "~,2f ~,2f ~,2f" r g b)

(bs:define-io-structure (size-option (:constructor %make-size-option))
  (font-size float32))

(defun make-size-option (font-size)
  (%make-size-option :font-size (float font-size 0f0)))

(define-print-method size-option "~,2f" font-size)

(bs:define-io-structure (heading-option (:constructor %make-heading-option))
  (level uint8))

(defun make-heading-option (level)
  (%make-heading-option :level level))

(define-print-method heading-option "~d" level)

(bs:define-io-structure (link-option (:constructor %make-link-option))
  (address (string uint16)))

(defun make-link-option (address)
  (%make-link-option :address address))

(define-print-method link-option "~a" address)

(bs:define-io-structure (target-option (:constructor %make-target-option))
  (address (string uint16)))

(defun make-target-option (address)
  (%make-target-option :address address))

(define-print-method target-option "~a" address)

(bs:define-io-alias markup-option
    (case uint8
      (#x01 :bold)
      (#x02 :italic)
      (#x03 :underline)
      (#x04 :strike)
      (#x05 :mono)
      (#x06 color-option)
      (#x07 size-option)
      (#x08 heading-option)
      (#x09 link-option)
      (#x0A target-option)))

(bs:define-io-structure (markup (:constructor %make-markup))
  (start uint64)
  (end uint64)
  (option markup-option))

(defun make-markup (start end option)
  (%make-markup :start start :end end :option option))

(bs:define-io-structure (text (:constructor %make-text))
  (markup-size uint64)
  (markup-options (vector markup uint32))
  (text (string uint64) :offset (the bs:index (+ 12 (bs:slot markup-size)))))

(define-print-method text "~a" text)

(defun make-text (text &rest markup)
  (let ((options (map 'vector
                      (lambda (x)
                        (etypecase x
                          (markup markup)
                          (cons 
                           (destructuring-bind (start end option &rest args) x
                             (assert (<= start end))
                             (make-markup :start start :end end
                                          :option (etypecase option
                                                    ((or color-option size-option heading-option link-option target-option)
                                                     option)
                                                    ((member :bold :italic :underline :strike :mono) option)
                                                    ((eql :color) (apply #'make-color-option args))
                                                    ((eql :size) (apply #'make-size-option args))
                                                    ((eql :heading) (apply #'make-heading-option args))
                                                    ((eql :link) (apply #'make-link-option args))
                                                    ((eql :target) (apply #'make-target-option args))))))))
                      markup)))
    (%make-text :markup-options options
                :markup-size (reduce #'+ options :key #'bs:octet-size)
                :text text)))

(define-accessors color-option r g b)
(define-accessors size-option font-size)
(define-accessors heading-option level)
(define-accessors link-option address)
(define-accessors target-option address)
(define-accessors markup start end option)
(define-accessors text markup text)
(define-delegates markup option r g b size level address)
