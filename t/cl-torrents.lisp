(in-package :cl-user)
(defpackage cl-torrents-test
  (:use :cl
        :cl-torrents
        :mockingbird
        :prove))
(in-package :cl-torrents-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-torrents)' in your Lisp.

(defun file-to-string (path)
  "Return the given file as a string."
    (with-open-file (stream path
                            :external-format :utf-8)
      (let ((data (make-string (file-length stream))))
        (read-sequence data stream)
        data)))

(plan nil)


(ok (torrents "matrix"))
;;
;; Unit tests.
;;

;; Load the search result html from a file.
(defparameter htmlpage (file-to-string #p"t/search-matrix.html"))

(with-dynamic-stubs ((dex:get htmlpage)) ;; the network call returns our known result.
  (ok (torrents "matrix") "torrent search ok"))

(finalize)
