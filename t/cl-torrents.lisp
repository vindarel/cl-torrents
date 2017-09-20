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
(defparameter htmlpage (file-to-string #p"search-matrix.html"))

;; Load the request to a details page.
(defparameter resultpage (file-to-string #p"t/search-matrix-result0.html"))

;; stubs: network calls return our known recorded html pages..
(with-dynamic-stubs ((dex:get htmlpage)
                     (cl-torrents::request-details resultpage))
  (ok (torrents "matrix") "torrent search ok")
  (is (cl-torrents::detail-page-url (elt cl-torrents::*last-search* 0))
      "https://piratebay.to/torrent/2297350/Matrix FRENCH DVDRIP 1999 COOL/"
      :test #'equalp
      "details-page-url returns the right url.")
  (ok (str:starts-with? "magnet" (magnet 0)) "magnet <i> returns the the magnet link from search result."))

(finalize)
