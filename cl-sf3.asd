(asdf:defsystem cl-sf3
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library to parse and create SF3 files."
  :homepage "https://shirakumo.org/docs/cl-sf3"
  :bug-tracker "https://shirakumo.org/projects/cl-sf3/issues"
  :source-control (:git "https://shirakumo.org/projects/cl-sf3.git")
  :depends-on (:cl-sf3/archive
               :cl-sf3/audio
               :cl-sf3/image
               :cl-sf3/log
               :cl-sf3/model
               :cl-sf3/physics-model
               :cl-sf3/table
               :cl-sf3/text
               :cl-sf3/vector-graphic)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-sf3/test))))

(asdf:defsystem cl-sf3/core
  :description "Core support utilities for cl-sf3"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "sf3")
               (:file "documentation"))
  :depends-on (:binary-structures
               :documentation-utils))

(asdf:defsystem cl-sf3/archive
  :description "Implementation of the SF3 archive format"
  :components ((:file "archive"))
  :depends-on (:cl-sf3/core
               :pathname-utils
               :filesystem-utils
               :file-attributes))

(asdf:defsystem cl-sf3/audio
  :description "Implementation of the SF3 audio format"
  :components ((:file "audio"))
  :depends-on (:cl-sf3/core))

(asdf:defsystem cl-sf3/image
  :description "Implementation of the SF3 image format"
  :components ((:file "image"))
  :depends-on (:cl-sf3/core))

(asdf:defsystem cl-sf3/log
  :description "Implementation of the SF3 log format"
  :components ((:file "log"))
  :depends-on (:cl-sf3/core))

(asdf:defsystem cl-sf3/model
  :description "Implementation of the SF3 model format"
  :components ((:file "model"))
  :depends-on (:cl-sf3/core))

(asdf:defsystem cl-sf3/physics-model
  :description "Implementation of the SF3 physics-model format"
  :components ((:file "physics-model"))
  :depends-on (:cl-sf3/core))

(asdf:defsystem cl-sf3/table
  :description "Implementation of the SF3 table format"
  :components ((:file "table"))
  :depends-on (:cl-sf3/core))

(asdf:defsystem cl-sf3/text
  :description "Implementation of the SF3 text format"
  :components ((:file "text"))
  :depends-on (:cl-sf3/core))

(asdf:defsystem cl-sf3/vector-graphic
  :description "Implementation of the SF3 vector-graphic format"
  :components ((:file "vector-graphic"))
  :depends-on (:cl-sf3/core))

(asdf:defsystem cl-sf3/test
  :description "Tests for cl-sf3"
  :serial T
  :components ((:file "test"))
  :depends-on (:cl-sf3
               :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.sf3.test)))
