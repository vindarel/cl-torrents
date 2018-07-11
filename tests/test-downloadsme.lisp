(in-package :cl-user)
(defpackage torrents-test.downloadsme
  (:use :cl
        :torrents.downloadsme
        :prove)
  (:import-from :torrents-test
                :file-to-string))

(in-package :torrents-test.downloadsme)

(plan nil)

;; Load the search result html from a file.
;; it's actually "matrix 1999", "matrix" only had an utf-8 error...
(defparameter htmlpage (file-to-string (asdf:system-relative-pathname :torrents #p"tests/assets/search-matrix-downloadsme.html")))

(defparameter *parsed-results* nil
  "List of plump nodes which parsed the test html.")

(defun get-nodes ()
  "Parse the test html, return a list of plump nodes ready to be queried by lquery."
  (setf *parsed-results* (coerce  (torrents.downloadsme::query
                                   (torrents.downloadsme::parse htmlpage))
                                  'list)))

;; Do this outside of a test to ease manual exploration.
(get-nodes)

(subtest "Testing torrentdownloads.me search results"
  (let ((node (first *parsed-results*)))
    (is (torrents.downloadsme::result-title node)
        " - The Matrix 1999 720p BRRip x264-x0r"
        "get a result title")

    (is  (torrents.downloadsme::result-href node)
         "/torrent/1664327908/-+The+Matrix+1999+720p+BRRip+x264-x0r"
         "get a result href")

    (is  (torrents.downloadsme::result-seeders node)
         7
         "get nb of seeders")

    (is  (torrents.downloadsme::result-leechers node)
         1
         "get nb of leechers")))


(finalize)
