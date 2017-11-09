(in-package :cl-user)
(defpackage cl-torrents-test
  (:use :cl
        :cl-torrents
        :mockingbird
        :prove)
  (:import-from :alexandria
                :assoc-value ;; get the val of an alist alone, not the (key val) couple.
                ))
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

;;
;; Unit tests.
;;

;; Load the search result html from a file.
(defparameter htmlpage (file-to-string #p"tests/assets/search-matrix.html"))

;; Load the request to a details page.
(defparameter resultpage (file-to-string #p"tests/assets/search-matrix-result0.html"))


;; stubs: network calls return our known recorded html pages..
(with-dynamic-stubs ((dex:get htmlpage)
                     (cl-torrents::request-details resultpage))

  (ok (with-output-to-string (out)
        (torrents "matrix" :stream out :log-stream nil)) "torrent search ok")

  (is 6 ;; 5 + 1 newline
      (length
       (str:lines
        (let ((cl-torrents::*nb-results* 5))
          (with-output-to-string (out)
            (torrents "matrix" :stream out :log-stream nil)))))
      "set the max nb of displayed results.")

  (is (assoc-value (elt cl-torrents::*last-search* 0) :href)
      "https://piratebay.to/torrent/2297350/Matrix FRENCH DVDRIP 1999 COOL/"
      :test #'equalp
      "we get the right href.")

  (ok (str:starts-with? "magnet" (magnet 0))
      "magnet <i> returns the the magnet link from search result.")

  (ok (str:starts-with? "198: Arturia"
                        ;; don't display the large output during the test.
                        (let ((cl-torrents::*nb-results* 1000))
                          (with-output-to-string (out)
                            (cl-torrents::display-results :results cl-torrents::*last-search* :stream out))))
      "Outputs results, reverse order.")

  (is 205
      (assoc-value (elt cl-torrents::*last-search* 0) :seeders)
      :test #'equalp
      "nb of peers.")

  (is 7
      (assoc-value (elt cl-torrents::*last-search* 0) :leechers)
      :test #'equalp
      "nb of leechers.")

  )

;; We can do the same with a macro, in order to isolate tests and to
;; factorize with-dynamic-stubs.
(defmacro with-mocked-search-results (body)
  `(with-dynamic-stubs ((dex:get htmlpage)
                        (cl-torrents::request-details resultpage))
     ,body))

;; Now we can run tests one by one.
(with-mocked-search-results
    (ok (with-output-to-string (out)
          (torrents "foo" :stream out :log-stream nil))
        "search ok"))

(with-mocked-search-results
    (is (assoc-value (elt cl-torrents::*last-search* 0) :href)
        "https://piratebay.to/torrent/2297350/Matrix FRENCH DVDRIP 1999 COOL/"
        :test #'equalp
        "detail-page-url returns the right url."))

(finalize)
