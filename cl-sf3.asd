#|
 This file is a part of SF3
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-sf3
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library to parse and create SF3 files."
  :homepage "https://shirakumo.github.io/sf3/"
  :bug-tracker "https://github.com/shirakumo/sf3/issues"
  :source-control (:git "https://github.com/shirakumo/sf3.git")
  :serial T
  :components ((:file "package")
               (:file "io-types")
               (:file "documentation"))
  :depends-on (:binary-structures
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :sf3-test))))
