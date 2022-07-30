#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(defun slot-type-type (type)
  (etypecase type
    ((eql :float) 'single-float)
    ((eql :double) 'double-float)
    ((eql :uint8) '(unsigned-byte 8))
    ((eql :uint16) '(unsigned-byte 16))
    ((eql :uint32) '(unsigned-byte 32))
    ((eql :uint64) '(unsigned-byte 64))
    ((eql :int8) '(signed-byte 8))
    ((eql :int16) '(signed-byte 16))
    ((eql :int32) '(signed-byte 32))
    ((eql :int64) '(signed-byte 64))
    ((eql :payload) T)
    (cons (ecase (first type)
            (object T)
            (map T)
            (string 'string)
            (vector `(simple-array ,(slot-type-type (third type)) (*)))))))

(defun slot-type-default (type)
  (etypecase type
    ((eql :float) 0f0)
    ((eql :double) 0d0)
    ((member :uint8 :uint16 :uint32 :uint64 :int8 :int16 :int32 :int64) 0)
    ((eql :payload) NIL)
    (cons (ecase (first type)
            (object NIL)
            (map NIL)
            (string "")
            (vector (make-array '(0) :element-type (slot-type-type (third type))))))))

(defun slot-type-parser (type)
  (etypecase type
    (real type)
    ((eql :payload) `(pos))
    (keyword `(ref ,type))
    (symbol type)
    (cons (case (first type)
            (object
             `(,(intern* '%parse (second type)) io))
            (map
             (destructuring-bind (type . maps) (rest type)
               `(ecase (ref ,type)
                  ,@maps)))
            (string
             `(parse-string io ,(slot-type-parser (second type))))
            (vector
             (destructuring-bind (type element) (rest type)
               `(map-into (make-array ,(slot-type-parser type) :element-type ',(slot-type-type element))
                          (lambda () ,(slot-type-parser element)))))
            (T type)))))

(defun slot-type-writer (name type)
  (etypecase type
    (real)
    ((eql :payload) `(write-payload io))
    (keyword `(set ,type ,name))
    (symbol)
    (cons (case (first type)
            (object
             `(,(intern* '%write (second type)) io ,name))
            (map
             (destructuring-bind (type . maps) (rest type)
               (if (constantp (second (first maps)))
                   `(case ,name
                      ,@(loop for (k v) in maps collect
                              `(,v (set ,type ,k))))
                   `(cond ,@(loop for (k v) in maps collect
                                  `((equal ,name ,v) (set ,type ,k)))))))
            (string
             `(progn
                (set ,(second type) (length ,name))
                (write-string io ,name)))
            (vector
             (destructuring-bind (type element) (rest type)
               (let ((el (gensym "ELEMENT")))
                 `(progn
                    ,(slot-type-writer name type)
                    (loop for ,el across ,name
                          do ,(slot-type-writer el element))))))
            (T)))))

(defmacro define-parsed-struct (name super &body header)
  (let ((make (intern* '%make name))
        (parse (intern* '%parse name))
        (write (intern* '%write name)))
    `(progn
       (defstruct (,name
                   ,@(when super `((:include ,super)))
                   (:constructor ,make (io))
                   (:copier NIL)
                   (:predicate NIL))
         ,@(loop for (slot type) in header
                 collect `(,slot ,(slot-type-default type) :type ,(slot-type-type type))))
       
       (define-io-fun ,parse (io)
         (let ((,name (,make io)))
           (symbol-macrolet ,(loop for (slot) in header
                                   collect `(,slot (,(intern* name slot) ,name)))
             ,@(loop for (slot type) in header
                     collect `(setf ,slot ,(slot-type-parser type))))
           ,name))

       (define-io-fun ,write (io ,name)
         (symbol-macrolet ,(loop for (slot) in header
                                 collect `(,slot (,(intern* name slot) ,name)))
           ,@(loop for (slot type) in header
                   collect (slot-type-writer slot type)))))))
