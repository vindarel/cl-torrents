(in-package :cl-user)
(defpackage torrents.1337
  (:use :cl)
  (:import-from :torrents.utils
                :join-for-query
                :sublist)
  (:export :torrents)
  )
(in-package :torrents.1337)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scraping 1337.to
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *base-url* "http://1337x.to")

(defparameter *source* :1337
  "Source short name for human presentation.")

(defparameter *search-url* "http://1337x.to/sort-search/{}/seeders/desc/1/"
  "Base url for a search. Sorted by seeders")

(defparameter *results-selector* "" "CSS selector to get a list of items inside the search results.")
(setf *results-selector* ".search-page tbody tr")

(defun request (url)
  (format t "dex:get ~a~&" url)
  (dex:get url))

(defun parse (html)
  (plump:parse html))

(defun query (parsed)
  (lquery:$ parsed *results-selector*))

(defun result-title (node)
  ;; title: second <a> in <td class="coll-1 name"
  (elt (lquery:$ node ".name" (text)) 0))

(defun result-href (node)
  (elt (lquery:$ node ".name a" (attr :href)) 1))

(defun result-seeders (node)
  ;; could be a better selector.
  (handler-case
      (parse-integer (elt (lquery:$ node ".seeds" (text)) 0))
    (error ()
      -1)))

(defun result-leechers (node)
  ;; could be a better selector.
  (handler-case
      (parse-integer (elt (lquery:$ node ".leeches" (text)) 0))
    (error ()
      -1)))

(defun torrents (words &key (stream t))
  "Return a list of..."
  (format stream "searching '~a' on ~a..." words *source*)
  (handler-case
      (let* ((query (str:join "+" words))
             (url (str:replace-all "{}" query *search-url*))
             (req (request url))
             (parsed (parse req))
             (results (query parsed))
             ;; (setf results (coerce results 'list))
             (toret (map 'list (lambda (node)
                                 `((:title . ,(result-title node))
                                   (:href . ,(str:concat *base-url* (result-href node)))
                                   (:seeders . ,(result-seeders node))
                                   (:leechers . ,(result-leechers node))
                                   (:source . ,*source*))
                                 )
                         results)))
        (format stream " found ~a results.~&" (length toret))
        toret)
    (error (c)
      (format stream " no results.~&")
      ;; xxx: logging
      (format *error-output* "error: ~a~&" c))))
