(defpackage #:org.shirakumo.sf3.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:sf3 #:org.shirakumo.sf3))
  (:export
   #:sf3
   #:compile-sample-files))

(in-package #:org.shirakumo.sf3.test)

(define-test sf3)

(defun output-sample-file (object name dir)
  (let ((file (merge-pathnames (format NIL "~a.~a" name (sf3:file-extension object))
                               (pathname-utils:subdirectory dir (string-downcase (type-of object))))))
    (sf3:write-sf3 object file :if-exists :supersede)))

(defun compile-sample-files (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
    (output (sf3:make-archive
             '(("Hello" :path "a")))
            "single-text")
    (output (sf3:make-archive
             '(("Hello" :path "a")
               ("There" :path "b")))
            "multi-text")

    (output (sf3:make-audio
             (make-array 1 :element-type 'single-float)
             :samplerate 44100 :channels 1)
            "f4-44100-1")
    (output (sf3:make-audio
             (make-array 2 :element-type 'single-float)
             :samplerate 44100 :channels 2)
            "f4-44100-2")
    (output (sf3:make-audio
             (make-array 1 :element-type '(unsigned-byte 8))
             :samplerate 44100 :channels 1)
            "u8-44100-1")

    (output (sf3:make-image
             (make-array 3 :element-type '(unsigned-byte 8))
             1 1 :pixel-type :rgb)
            "rgb-u8-1x1")
    (output (sf3:make-image
             (make-array 1 :element-type 'single-float)
             1 1 :pixel-type :v)
            "v-f4-1x1")
    (output (sf3:make-image
             (make-array 16 :element-type 'single-float)
             4 4 :pixel-type :v)
            "v-f4-4x4")
    (output (sf3:make-image
             (make-array 1 :element-type '(unsigned-byte 8))
             1 1 :pixel-type :v)
            "v-u8-1x1")

    (output (sf3:make-model
             '(0 1 2 2 1 3) 
             '(0.0 0.0 0.0 0.0 0.0
               1.0 0.0 0.0 1.0 0.0
               0.0 1.0 0.0 0.0 1.0
               1.0 1.0 0.0 1.0 1.0)
             :vertex-attributes '(:position :uv)
             :material '(:albedo "texture.png"))
            "textured-quad")
    (output (sf3:make-model
             '(0 1 2 2 1 3)
             '(0.0 0.0 0.0
               1.0 0.0 0.0
               0.0 1.0 0.0
               1.0 1.0 0.0))
            "quad")
    (output (sf3:make-model
             '(0 1 2)
             '(0.0 0.0 0.0
               1.0 0.0 0.0
               0.5 1.0 0.0))
            "triangle")
    (output (sf3:make-model
             () ())
            "empty")

    (output (sf3:make-physics-model
             1 '(1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)
             (sf3:make-mesh '(0.0 0.0 0.0
                              1.0 0.0 0.0
                              0.5 1.0 0.5
                              0.5 0.0 1.0)))
            "mesh")
    (output (sf3:make-physics-model
             1 '(1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)
             (sf3:make-cylinder 1 1 1)
             (sf3:make-ellipsoid 1 1 1 :transform
                                 '(1.0 0.0 0.0 0.0
                                   0.0 1.0 0.0 1.0
                                   0.0 0.0 1.0 0.0
                                   0.0 0.0 0.0 1.0)))
            "combine")
    (output (sf3:make-physics-model
             1 '(1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)
             (sf3:make-box 1 1 1))
            "box")

    (output (sf3::%make-log
             :start-time (sf3:universal-to-unix-time (get-universal-time))
             :end-time (sf3:universal-to-unix-time (get-universal-time))
             :chunks (make-array 0 :adjustable T :fill-pointer T))
            "empty")
    (output (sf3:make-log
             :entries 0)
            "empty-chunk")
    (output (sf3:log
             (sf3:make-log :entries 1) "Hello" :severity 10 :source "sf3" :category "test")
            "filled-message")
    (output (sf3:log
             (sf3:make-log :entries 2) "Hello")
            "partial-chunk")
    (output (sf3:log
             (sf3:log (sf3:make-log :entries 1) "Hello") "There")
            "two-chunks")
    (output (sf3:log
             (sf3:make-log :entries 1) "Hello")
            "one-message")

    (output (sf3:make-table
             '(("Position" :float32 3))
             '(((1f0 0f0 0f0))))
            "multiple-elements")
    (output (sf3:make-table
             '("Name" ("Age" :uint8))
             '(("Yukari" 31)))
            "two-columns")
    (output (sf3:make-table
             '("Name")
             '(("Yukari") ("Hafner")))
            "two-rows")
    (output (sf3:make-table
             '("Name")
             '(("Yukari")))
            "one-row")
    (output (sf3:make-table
             '("Name")
             '())
            "empty")

    (output (sf3:make-vector-graphic
             100 100
             (list 
              (sf3:make-text-shape 0 12 "Hello there!")))
            "text")
    (output (sf3:make-vector-graphic
             100 100
             (list 
              (sf3:make-rectangle 0 0 50 50 :fill-color (sf3:make-color 1 0 0))
              (sf3:make-circle 0 0 100 100 :fill-color (sf3:make-color 0 1 0 0.5))
              (sf3:make-line (list (sf3:make-point 0 0) (sf3:make-point 100 100)))))
            "blend")
    (output (sf3:make-vector-graphic
             100 100
             (list (sf3:make-line (list (sf3:make-point 0 0) (sf3:make-point 100 100)))))
            "diagonal")
    (output (sf3:make-vector-graphic
             100 100
             (list (sf3:make-circle 0 0 100 100)))
            "circle")
    (output (sf3:make-vector-graphic
             100 100
             (list (sf3:make-rectangle 0 0 100 100)))
            "square")))
