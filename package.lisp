#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.sf3
  (:use #:cl #:org.shirakumo.binary-structures.types)
  (:shadow #:log #:read-line #:write-line)
  (:local-nicknames
   (#:bs #:org.shirakumo.binary-structures))
  (:export))
