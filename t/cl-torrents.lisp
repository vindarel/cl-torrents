(in-package :cl-user)
(defpackage cl-torrents-test
  (:use :cl
        :cl-torrents
        :prove))
(in-package :cl-torrents-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-torrents)' in your Lisp.

(plan nil)

;; blah blah blah.

(ok (torrents "matrix"))

(finalize)
