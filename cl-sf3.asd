(asdf:defsystem cl-sf3
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library to parse and create SF3 files."
  :homepage "https://shirakumo.github.io/cl-sf3/"
  :bug-tracker "https://github.com/shirakumo/cl-sf3/issues"
  :source-control (:git "https://github.com/shirakumo/cl-sf3.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "archive")
               (:file "audio")
               (:file "image")
               (:file "log")
               (:file "model")
               (:file "physics-model")
               (:file "table")
               (:file "text")
               (:file "vector-graphic")
               (:file "sf3")
               (:file "documentation"))
  :depends-on (:binary-structures
               :pathname-utils
               :filesystem-utils
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-sf3/test))))

(asdf:defsystem cl-sf3/test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for cl-sf3"
  :serial T
  :components ((:file "test"))
  :depends-on (:cl-sf3
               :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.sf3.test)))
