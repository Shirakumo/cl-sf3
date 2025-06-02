(asdf:load-system :staple-markless)
(defmethod staple:subsystems ((_ (eql (asdf:find-system :cl-sf3)))))
(defmethod staple:packages ((_ (eql (asdf:find-system :cl-sf3)))) (list (find-package :ORG.SHIRAKUMO.SF3)))
