(in-package :cl-user)
(defpackage kat
  (:use :cl)
  (:import-from :torrents.models
                :make-torrent)
  (:import-from :torrents.utils
                :join-for-query
                :sublist)
  (:export :torrents)
  )
(in-package :kat)

;; !!!!!!!!!!!!!!!!!!!
;; katcr.co is down :(
;; !!!!!!!!!!!!!!!!!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scraping katcr.co.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *base-url* "http://katcr.co")

(defparameter *search-url* "http://katcr.co/new/search-torrents.php?search={}&sort=seeders&order=desc"
  "Base url for a search. Sorted by seeders")

(defparameter *results-selector* "" "CSS selector to get a list of items inside the search results.")
(setf *results-selector* ".t-row")

(defun request (url)
  (dex:get url))

(defun parse (html)
  (plump:parse html))

(defun query (parsed)
  ;; debug example: break, inspect "parsed" with a click or by
  ;; doing sthg with the local "kat::parsed" (eval code with "e").
  ;; (break)
  (lquery:$ parsed *results-selector*))

(defun result-title (node)
  (elt (lquery:$ node ".cellMainLink" (text)) 0))

(defun result-href (node)
  (elt (lquery:$ node ".cellMainLink" (attr :href)) 0))

(defun result-seeders (node)
  ;; could be a better selector.
  (handler-case
      (parse-integer (elt (lquery:$ node ".ttable_col2" (text)) 1))
    (error ()
      -1)))

(defun result-leechers (node)
  ;; could be a better selector.
  (handler-case
      (parse-integer (elt (lquery:$ node ".ttable_col1" (text)) 2))
    (error ()
      -1)))

(defun torrents (words &key (stream t))
  "Return a list of..."
  (format stream "searching '~a' on Kat... " words)
  (handler-case
      (let* ((query (str:join "+" words))
             (url (str:replace-all "{}" query *search-url*))
             (req (request url))
             (parsed (parse req))
             (results (query parsed))
             ;; (setf results (coerce results 'list))
             (toret (map 'list (lambda (node)
                                 (make-torrent
                                  :title (result-title node)
                                  :href (str:concat *base-url* "/new/" (result-href node))
                                  :seeders (result-seeders node)
                                  :leechers (result-leechers node)
                                  :source :kat))
                         results)))
        (format stream " found ~a results.~&" (length toret))
        toret)
    (usocket:connection-refused-error ()
      (uiop:format! *error-output* "~&error searching on ~a: ~a.~&"
                    "Kat"
                    (cl-ansi-text:red "the site is unreachable")))
    (error ()
      (format stream " no results.~&"))))
