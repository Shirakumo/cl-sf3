#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.sf3)

(defun intern* (&rest parts)
  (intern (format NIL "~{~a~^-~}" parts)))

(defmacro with-cleanup-on-failure (cleanup &body body)
  (let ((failed (gensym "FAILED")))
    `(let ((,failed T))
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setf ,failed NIL))
         (when ,failed
           ,cleanup)))))
