#|
  This file is a part of cl-torrents project.
|#

(in-package :cl-user)
(defpackage torrents-test-asd
  (:use :cl :asdf))
(in-package :torrents-test-asd)

(defsystem torrents-test
  :author ""
  :license ""
  :depends-on (:torrents
               :mockingbird
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-torrents"))))
  :description "Test system for cl-torrents"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
