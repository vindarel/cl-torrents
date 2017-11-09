(in-package :cl-user)
(defpackage torrentcd
  (:use :cl)
  (:import-from :cl-torrents.utils
                :join-for-query)
  (:import-from :alexandria
                :flatten)
  (:export :torrents)
  )
(in-package :torrentcd)

;; This scraping is fragile... good live tests are mandatory.
;; and lacks error handling !

(defparameter *search-url* "https://torrent.cd/torrents/search/"
  "Base url for a search. A POST request is necessary to get sorted results by seeds")

(defun request (search &optional (url *search-url*))
()  "Request to torrent.cd. POST request in order to get results sorted by seeders."
  (dex:post url
            :content `(("search-keywords" . ,search)
                       ("catid" . 0)
                       ("type" . "allwords")
                       ("sortby" . "sed"))))

(defun parse (html)
  (plump:parse html))

(defun query (parsed)
  ;; There are two tables in the page with the same CSS.
  (let* ((res (lquery:$ parsed ".data"))
         (res (elt res 1))
         (res (lquery:$ res "tr")) ;; we also get the table headers...
         (res (subseq res 1)))
    res))

(defun result-href (node)
  (elt (lquery:$ node ".desc a" (attr :href))
       0))

(defun result-seeders (node)
  (parse-integer (elt (lquery:$ node "td" (text))
                      5)))

(defun result-title (node)
  (elt (lquery:$ node ".desc a" (text))
       0))

(defun torrents (words &key (stream t))
  "Return a list of alists with title, href, and seeders."
  (format stream "searching torrent.cd…")
  (let* ((query (join-for-query words))
         (req (request query))
         (parsed (parse req))
         (results (query parsed))
         (toret (map 'list (lambda (node)
                             `((:title . ,(result-title node))
                               (:href . ,(result-href node))
                               (:seeders . ,(result-seeders node)))
                             )
                     results)))
    (format stream " found ~a results." (length toret))
    toret))
