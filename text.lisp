(in-package #:org.shirakumo.sf3)

(bs:define-io-structure color-option
  (r float32)
  (g float32)
  (b float32))

(define-print-method color-option "~,2f ~,2f ~,2f" r g b)

(bs:define-io-structure size-option
  (size float32))

(define-print-method size-option "~,2f" size)

(bs:define-io-structure heading-option
  (level uint8))

(define-print-method heading-option "~d" level)

(bs:define-io-structure link-option
  (address (string uint16)))

(define-print-method link-option "~a" address)

(bs:define-io-structure target-option
  (address (string uint16)))

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

(bs:define-io-structure markup
  (start uint64)
  (end uint64)
  (option markup-option))

(bs:define-io-structure (text (:constructor %make-text))
  (markup-size uint64)
  (markup-options (vector markup uint32))
  (text (string uint64) :offset (the bs:index (+ 12 (bs:slot markup-size)))))

(define-print-method text "~a" text)

(defun make-text (text &rest markup)
  (let ((options (map 'vector
                      (lambda (x)
                        (destructuring-bind (start end option &rest args) x
                          (assert (<= start end))
                          (make-markup :start start :end end
                                       :option (ecase option
                                                 ((:bold :italic :underline :strike :mono) option)
                                                 (:color
                                                  (destructuring-bind (r g b) args
                                                    (make-color-option :r (float r 0f0)
                                                                       :g (float g 0f0)
                                                                       :b (float b 0f0))))
                                                 (:size
                                                  (destructuring-bind (size) args
                                                    (make-size-option :size (float size 0f0))))
                                                 (:heading
                                                  (destructuring-bind (level) args
                                                    (make-heading-option :level level)))
                                                 (:link
                                                  (destructuring-bind (address) args
                                                    (make-link-option :address address)))
                                                 (:target
                                                  (destructuring-bind (address) args
                                                    (make-target-option :address address)))))))
                      markup)))
    (%make-text :markup-options options
                :markup-size (reduce #'+ options :key #'bs:octet-size)
                :text text)))

(define-accessors color-option r g b)
(define-accessors size-option size)
(define-accessors heading-option level)
(define-accessors link-option address)
(define-accessors target-option address)
(define-accessors text markup text)
