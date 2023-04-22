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
  (:export
   #:archive-meta-entry
   #:archive
   #:read-archive
   #:write-archive
   #:audio
   #:read-audio
   #:write-audio
   #:image
   #:read-image
   #:write-image
   #:log-entry
   #:read-log-entry
   #:write-log-entry
   #:log-chunk
   #:read-log-chunk
   #:write-log-chunk
   #:log
   #:read-log
   #:write-log
   #:model
   #:read-model
   #:write-model
   #:text
   #:read-text
   #:write-text
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
   #:read-vector-graphic
   #:write-vector-graphic))
