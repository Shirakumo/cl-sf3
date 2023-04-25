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
   #:make-archive-meta-entry
   #:archive
   #:make-archive
   #:audio
   #:make-audio
   #:image
   #:make-image
   #:log-entry
   #:make-log-entry
   #:log-chunk
   #:make-log-chunk
   #:log
   #:make-log
   #:model
   #:make-model
   #:color-option
   #:make-color-option
   #:size-option
   #:make-size-option
   #:heading-option
   #:make-heading-option
   #:link-option
   #:make-link-option
   #:target-option
   #:make-target-option
   #:text
   #:make-text
   #:color
   #:make-color
   #:point
   #:make-point
   #:size
   #:make-size
   #:line
   #:make-line
   #:rectangle
   #:make-rectangle
   #:circle
   #:make-circle
   #:polygon
   #:make-polygon
   #:curve
   #:make-curve
   #:text-shape
   #:make-text-shape
   #:vector-graphic
   #:make-vector-graphic
   #:sf3-file)
  (:export
   #:medification-time
   #:checksum
   #:mime-type
   #:path
   #:meta-entries
   #:files
   #:samplerate
   #:channels
   #:sample-format
   #:samples
   #:image
   #:width
   #:height
   #:depth
   #:data
   #:size
   #:time
   #:severity
   #:source
   #:category
   #:message
   #:entries
   #:start-time
   #:chunks
   #:textures
   #:faces
   #:color
   #:level
   #:address
   #:markup
   #:text
   #:r
   #:g
   #:b
   #:a
   #:x
   #:y
   #:w
   #:h
   #:thickness
   #:points
   #:fill-color
   #:outline-color
   #:outline-thickness
   #:location
   #:font
   #:font-size
   #:instructions
   #:content
   #:file-extension
   #:mime-type
   #:read-sf3
   #:write-sf3
   #:tell-sf3))
