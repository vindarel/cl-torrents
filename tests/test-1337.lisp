(in-package :cl-user)
(defpackage torrents-test.1337
  (:use :cl
        :torrents.1337
        :mockingbird
        :prove)
  (:import-from :torrents-test
                :file-to-string))

(in-package :torrents-test.1337)

(plan nil)

;; Load the search result html from a file.
(defparameter htmlpage (file-to-string (asdf:system-relative-pathname :torrents #p"tests/assets/search-matrix-1337.html")))

(defparameter *parsed-results* nil
  "List of plump nodes which parsed the test html.")

(defun get-nodes ()
  "Parse the test html, return a list of plump nodes ready to be queried by lquery."
  (setf *parsed-results* (coerce  (torrents.1337::query (plump:parse htmlpage))
                                  'list)))


(get-nodes)

(subtest "Testing 1337 search results"
  (is  (torrents.1337::result-title (first *parsed-results*))
       "The Matrix (1999) 720p BrRip x264 - 700mb - YIFY 1"
       "get a result title")

  (is  (torrents.1337::result-href (first *parsed-results*))
       "/torrent/240847/The-Matrix-1999-720p-BrRip-x264-700mb-YIFY/"
       "get a result href")

  (is  (torrents.1337::result-seeders (first *parsed-results*))
       513
       "get nb of seeders")

  (is  (torrents.1337::result-leechers (first *parsed-results*))
       45
       "get nb of leechers"))


(finalize)
