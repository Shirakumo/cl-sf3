#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.sf3
  (:use #:cl #:org.shirakumo.binary-structures.types)
  (:shadow #:log #:read-line #:write-line #:time)
  (:local-nicknames
   (#:bs #:org.shirakumo.binary-structures))
  (:export
   #:archive-meta-entry
   #:archive
   #:audio
   #:image
   #:log-entry
   #:log-chunk
   #:log
   #:model
   #:color-option
   #:size-option
   #:heading-option
   #:link-option
   #:target-option
   #:text
   #:color
   #:point
   #:size
   #:line
   #:rectangle
   #:circle
   #:polygon
   #:curve
   #:text-shape
   #:vector-graphic
   #:sf3-file))
