#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.sf3
  (:use #:cl)
  (:shadow #:vector #:set #:log #:write-string)
  (:local-nicknames
   (#:mmap #:org.shirakumo.fraf.trial.mmap))
  (:export))
