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
  (let ((file (merge-pathnames (format NIL "~a.~a" name (sf3:file-extension object)) dir)))
    (sf3:write-sf3 object file :if-exists :supersede)))

(defun compile-sample-files/archive (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
    (output (sf3:make-archive
             '(("Hello" :path "a" :modification-time #.(encode-universal-time 0 0 0 1 1 2025 0))))
            "single-text")
    (output (sf3:make-archive
             '(("Hello" :path "a" :modification-time #.(encode-universal-time 0 0 0 1 1 2025 0))
               ("There" :path "b" :modification-time #.(encode-universal-time 0 0 0 18 11 1993 0))))
            "multi-text")))

(defun compile-sample-files/audio (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
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
            "u8-44100-1")))

(defun compile-sample-files/image (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
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
            "v-u8-1x1")))

(defun compile-sample-files/model (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
    (let ((a (sf3:make-model
              '(0 1 2 2 1 3)
              '(0.0 0.0 0.0 0.0 0.0
                1.0 0.0 0.0 1.0 0.0
                0.0 1.0 0.0 0.0 1.0
                1.0 1.0 0.0 1.0 1.0)
              :vertex-attributes '(:position :uv)))
          (b (sf3:make-model
              '(0 1 2 2 1 3)
              '(2.0 0.0 0.0 0.0 0.0
                3.0 0.0 0.0 1.0 0.0
                2.0 1.0 0.0 0.0 1.0
                3.0 1.0 0.0 1.0 1.0)
              :vertex-attributes '(:position :uv))))
      (output (sf3:make-archive
               `((,(sf3:write-sf3 a 'vector)
                  :path "a"
                  :mime-type "model/x.sf3"
                  :modification-time #.(encode-universal-time 0 0 0 1 1 2025 0))
                 (,(sf3:write-sf3 b 'vector)
                  :path "b"
                  :mime-type "model/x.sf3"
                  :modification-time #.(encode-universal-time 0 0 0 1 1 2025 0))))
              "multiple"))
    (output (sf3:make-model
             '(0 1 2 2 1 3)
             '(0.0 0.0 0.0  0.0 0.0  0.0 0.0 -1.0
               1.0 0.0 0.0  1.0 0.0  0.0 0.0 -1.0
               0.0 1.0 0.0  0.0 1.0  0.0 0.0 -1.0
               1.0 1.0 0.0  1.0 1.0  0.0 0.0 -1.0)
             :vertex-attributes '(:position :uv :normal)
             :material '(:albedo "albedo.png"
                         :normal "normal.png"
                         :metallic "metallic.png"))
            "pbr-quad")
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
            "empty")))

(defun compile-sample-files/physics-model (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
    (output (sf3:make-physics-model
             1 '(1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)
             (sf3:make-mesh '(0.0 0.0 0.0
                              1.0 0.0 0.0
                              0.5 1.0 0.5
                              0.5 0.0 1.0))
             (sf3:make-ellipsoid 1 1 1)
             (sf3:make-box 0.5 0.5 2)
             (sf3:make-cylinder 0.2 2.0 10.0)
             (sf3:make-pill 1.5 0.0 5.0))
            "all-shapes")
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
            "box")))

(defun compile-sample-files/log (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
    (output (sf3::%make-log
             :start-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
             :end-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
             :chunks (make-array 0 :adjustable T :fill-pointer T))
            "empty")
    (output (sf3:make-log
             :start-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
             :end-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
             :entries 0)
            "empty-chunk")
    (output (let ((log (sf3:make-log
                        :start-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
                        :end-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
                        :entries 1)))
              (sf3:log log "Hello" :severity 10 :source "sf3" :category "test" :time 10))
            "filled-message")
    (output (let ((log (sf3:make-log
                        :start-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
                        :entries 2)))
              (sf3:log log "Hello" :time 5))
            "partial-chunk")
    (output (let ((log (sf3:make-log
                        :start-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
                        :end-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
                        :entries 1)))
              (sf3:log log "Hello" :time 1)
              (sf3:log-append-chunk log :entries 1)
              (sf3:log log "There" :time 2))
            "two-chunks")
    (output (let ((log (sf3:make-log
                        :start-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
                        :end-time (sf3:universal-to-unix-time (encode-universal-time 0 0 0 1 1 2025 0))
                        :entries 1)))
              (sf3:log log "Hello" :time 0))
            "one-message")))

(defun compile-sample-files/table (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
    (output (sf3:make-table
             '(("u8" :uint8) ("u16" :uint16) ("u32" :uint32) ("u64" :uint64)
               ("s8" :sint8) ("s16" :sint16) ("s32" :sint32) ("s64" :sint64)
               ("f2" :float16) ("f4" :float32) ("f8" :float64)
               ("str" :string) ("t" :timestamp) ("t+" :high-resolution-timestamp)
               ("b" :boolean))
             '((8 16 32 64 -8 -16 -32 -64 #x4C00 32f0 64d0 "String"
                #.(encode-universal-time 0 0 0 1 1 2025)
                #.(encode-universal-time 0 0 0 1 1 2025)
                T)))
            "all-types")
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
            "empty")))

(defun compile-sample-files/text (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
    (output (sf3:make-text
             "Hello")
            "plain")
    (output (sf3:make-text
             "Hello there"
             '(0 5 :bold)
             '(6 11 :size 10.0))
            "markup")
    (output (sf3:make-text
             "bold italic underline strike mono color size heading link target font"
             '( 0  4 :bold)
             '( 5 11 :italic)
             '(12 21 :underline)
             '(22 28 :strike)
             '(29 33 :mono)
             '(34 39 :color 1 0 0)
             '(40 44 :size 12)
             '(45 52 :heading 1)
             '(53 57 :link "https://shirakumo.org")
             '(58 64 :target "target")
             '(65 69 :font "ComicSansMs"))
            "all-options")))

(defun compile-sample-files/vector-graphic (dir)
  (flet ((output (object name)
           (output-sample-file object name dir)))
    (output (sf3:make-vector-graphic
             100 100
             (list
              (sf3:make-line (list (sf3:make-point 5 5) (sf3:make-point 15 5) (sf3:make-point 15 15)))
              (sf3:make-rectangle 20 20 10 5)
              (sf3:make-circle 0 20 5 10)
              (sf3:make-polygon (list (sf3:make-point 495 495) (sf3:make-point 480 480) (sf3:make-point 470 495)))
              (sf3:make-curve (list (sf3:make-point 450 450) (sf3:make-point 450 430)
                                    (sf3:make-point 430 470) (sf3:make-point 430 430)))
              (sf3:make-text-shape 250 250 "SF3")))
            "all-instructions")
    (output (sf3:make-vector-graphic
             100 100
             (list
              (sf3:make-curve (list (sf3:make-point 0 0) (sf3:make-point 0 100)
                                    (sf3:make-point 100 100) (sf3:make-point 100 0)))))
            "curve")
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

(defun compile-sample-files (dir)
  (compile-sample-files/archive (pathname-utils:subdirectory dir "archive"))
  (compile-sample-files/audio (pathname-utils:subdirectory dir "audio"))
  (compile-sample-files/image (pathname-utils:subdirectory dir "image"))
  (compile-sample-files/model (pathname-utils:subdirectory dir "model"))
  (compile-sample-files/physics-model (pathname-utils:subdirectory dir "physics-model"))
  (compile-sample-files/log (pathname-utils:subdirectory dir "log"))
  (compile-sample-files/table (pathname-utils:subdirectory dir "table"))
  (compile-sample-files/text (pathname-utils:subdirectory dir "text"))
  (compile-sample-files/vector-graphic (pathname-utils:subdirectory dir "vector-graphic")))
