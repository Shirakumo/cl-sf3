(in-package #:org.shirakumo.sf3)

;; toolkit.lisp
(docs:define-docs
  (function unix-to-universal-time
    "Convert a UNIX timestamp to a Common Lisp universal-time timestamp")

  (function universal-time-to-unix
    "Convert a Common Lisp universal-time timestamp to a UNIX timestamp"))

;; archive.lisp
(docs:define-docs
  (type archive-meta-entry
    "")
  
  (function make-archive-meta-entry
    "")
  
  (type archive
    "")
  
  (function make-archive
    "")
  
  (function modification-time
    "")
  
  (function checksum
    "")
  
  (function mime-type
    "")
  
  (function path
    "")
  
  (function files
    "")
  
  (function add-file
    "")
  
  (function extract-file
    ""))

;; audio.lisp
(docs:define-docs
  (type audio
    "")
  
  (function make-audio
    "")
  
  (function duration
    "")
  
  (function samplerate
    "")
  
  (function channels
    "")
  
  (function sample-format
    "")
  
  (function samples
    ""))

;; image.lisp
(docs:define-docs
  (type image
    "")
  
  (function make-image
    "")
  
  (function pixel-type
    "")
  
  (function width
    "")
  
  (function height
    "")
  
  (function depth
    "")
  
  (function data
    ""))

;; log.lisp
(docs:define-docs
  (type log-entry
    "")
  
  (function make-log-entry
    "")
  
  (type log-chunk
    "")
  
  (function make-log-chunk
    "")
  
  (type log
    "")
  
  (function make-log
    "")
  
  (function size
    "")
  
  (function time
    "")
  
  (function severity
    "")
  
  (function source
    "")
  
  (function category
    "")
  
  (function message
    "")
  
  (function entries
    "")
  
  (function start-time
    "")
  
  (function end-time
    "")
  
  (function chunks
    "")
  
  (function log-append-chunk
    "")
  
  (function log-append-entry-extend
    ""))

;; model.lisp
(docs:define-docs
  (type model
    "")
  
  (function make-model
    "")
  
  (function vertex-attributes
    "")
  
  (function vertex-stride
    "")
  
  (function texture-types
    "")
  
  (function textures
    "")
  
  (function faces
    "")
  
  (function vertices
    ""))

;; physics-model.lisp
(docs:define-docs
  (type ellipsoid
    "")
  
  (function make-ellipsoid
    "")
  
  (type box
    "")
  
  (function make-box
    "")
  
  (type cylinder
    "")
  
  (function make-cylinder
    "")
  
  (type pill
    "")
  
  (function make-pill
    "")
  
  (type mesh
    "")
  
  (function make-mesh
    "")
  
  (type shape
    "")
  
  (function make-shape
    "")
  
  (type physics-model
    "")
  
  (function make-physics-model
    "")
  
  (function bottom-radius
    "")
  
  (function top-radius
    "")
  
  (function vertices
    "")
  
  (function transform
    "")
  
  (function data
    "")
  
  (function mass
    "")
  
  (function tensor
    "")
  
  (function shapes
    ""))

;; table.lisp
(docs:define-docs
  (type column-spec
    "")
  
  (function make-column-spec
    "")
  
  (function kind
    "")
  
  (function column-spec-element-size
    "")
  
  (function column-spec-element-count
    "")
  
  (type table
    "")
  
  (function make-table
    "")
  
  (function column-count
    "")
  
  (function row-count
    "")
  
  (function column-specs
    "")
  
  (function row-data
    "")
  
  (function row
    "")
  
  (function cell
    ""))

;; text.lisp
(docs:define-docs
  (type color-option
    "")
  
  (function make-color-option
    "")
  
  (type size-option
    "")
  
  (function make-size-option
    "")
  
  (type heading-option
    "")
  
  (function make-heading-option
    "")
  
  (type link-option
    "")
  
  (function make-link-option
    "")
  
  (type target-option
    "")
  
  (function make-target-option
    "")
  
  (type markup
    "")
  
  (function make-markup
    "")
  
  (type text
    "")
  
  (function make-text
    "")
  
  (function r
    "")
  
  (function g
    "")
  
  (function b
    "")
  
  (function level
    "")
  
  (function address
    "")
  
  (function text
    ""))

;; vector-graphic.lisp
(docs:define-docs
  (type color
    "")
  
  (function make-color
    "")
  
  (type point
    "")
  
  (function make-point
    "")

  (type size
    "")
  
  (function make-size
    "")
  
  (type shape-fill
    "")
  
  (function make-shape-fill
    "")
  
  (type shape-bounds
    "")
  
  (function make-shape-bounds
    "")
  
  (type line
    "")
  
  (function make-line
    "")
  
  (type rectangle
    "")
  
  (function make-rectangle
    "")
  
  (type circle
    "")
  
  (function make-circle
    "")
  
  (type polygon
    "")
  
  (function make-polygon
    "")
  
  (type curve
    "")
  
  (function make-curve
    "")
  
  (type text-shape
    "")
  
  (function make-text-shape
    "")
  
  (type vector-graphic
    "")
  
  (function make-vector-graphic
    "")
  
  (function a
    "")
  
  (function x
    "")
  
  (function y
    "")
  
  (function w
    "")
  
  (function h
    "")
  
  (function fill-color
    "")
  
  (function outline-color
    "")
  
  (function outline-thickness
    "")
  
  (function thickness
    "")
  
  (function bounds
    "")
  
  (function fill
    "")
  
  (function points
    "")
  
  (function font
    "")
  
  (function font-size
    "")
  
  (function instructions
    ""))

;; sf3.lisp
(docs:define-docs
  (type sf3-file-header
    "")
  
  (function make-sf3-file-header
    "")
  
  (function file-extension
    "")
  
  (function read-sf3
    "")
  
  (function write-sf3
    "")
  
  (function tell-sf3
    ""))
