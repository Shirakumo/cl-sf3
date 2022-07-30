#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(defvar *type-info-lookup* (make-hash-table :test 'equal))

(defclass type-info () ())

(defgeneric type-fields (type))
(defgeneric type-ref (type var ref-type))
(defgeneric type-ref* (type var ref-type))
(defgeneric type-set (type var ref-type value))
(defgeneric type-pos (type var))
(defgeneric type-epilogue (type var))

(defun type-info (name &optional (errorp T))
  (or (gethash name *type-info-lookup*)
      (when errorp (error "No type-info with name ~s" name))))

(defun (setf type-info) (value name)
  (if value
      (setf (gethash name *type-info-lookup*) value)
      (remhash name *type-info-lookup*))
  value)

(defmethod type-fields ((type symbol))
  (type-fields (type-info type)))

(defmethod type-ref ((type symbol) var ref-type)
  (type-ref (type-info type) var ref-type))

(defmethod type-ref* ((type symbol) var ref-type)
  (type-ref* (type-info type) var ref-type))

(defmethod type-set ((type symbol) var ref-type value)
  (type-set (type-info type) var ref-type value))

(defmethod type-pos ((type symbol) var)
  (type-pos (type-info type) var))

(defmethod type-epilogue ((type symbol) var)
  (type-epilogue (type-info type) var))

(defmethod type-fields ((type type-info))
  ())

(defmethod type-epilogue ((type type-info) var)
  ())

(defun ref-type-size (ref-type)
  (ecase ref-type
    ((:int8 :uint8) 1)
    ((:int16 :uint16) 2)
    ((:int32 :uint32 :float) 4)
    ((:int64 :uint64 :double) 8)))

(defmacro define-type-info (name type &body forms)
  `(progn
     (defclass ,name (type-info)
       ())
     
     (setf (type-info ',type) (make-instance ',name))

     ,@(loop for (kind args . body) in forms
             collect `(defmethod ,(ecase kind
                                    ((:fields type-fields) 'type-fields)
                                    ((:ref type-ref) 'type-ref)
                                    ((:ref* type-ref*) 'type-ref*)
                                    ((:set type-set) 'type-set)
                                    ((:pos type-pos) 'type-pos)
                                    ((:epilogue type-epilogue) 'type-epilogue))
                          ((,name ,name) ,@args)
                        ,@body))))

(defmacro with-accessors-for ((var type) &body body)
  `(let ,(loop for field in (type-fields type)
               collect `(,field (,field ,var)))
     (macrolet ((ref (type)
                  (type-ref ',type ',var type))
                (ref* (type)
                  (type-ref* ',type ',var type))
                (set (type value)
                  (type-set ',type ',var type value))
                (pos ()
                  (type-pos ',type ',var)))
       (multiple-value-prog1 (progn ,@body)
         ,(type-epilogue type var)))))

(defmacro define-typed-fun (name type args &body body)
  `(defun ,name ,args
     (declare (type ,type ,(first args)))
     (with-accessors-for (,(first args) ,type)
       ,@body)))

(defmacro define-dispatched-fun (name types args &body body)
  (let ((names (loop for type in types collect (intern* '% type name))))
    `(progn
       ,@(loop for type in types
               for name in names
               collect `(define-typed-fun ,name ,type ,args
                          ,@body))

       (defun ,name ,args
         (etypecase ,(first args)
           ,@(loop for type in types
                   for name in names
                   collect `(,type (,name ,@args)))))

       ;; TODO: transforms
       (progn))))
