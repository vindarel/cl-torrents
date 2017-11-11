(in-package :cl-user)
(defpackage kat
  (:use :cl)
  (:import-from :alexandria
                :flatten)
  (:import-from :cl-torrents.utils
                :join-for-query
                :sublist)
  (:export :torrents
           :find-magnet-link)
  )
(in-package :kat)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scraping katcr.co.
;; Ok but I don't find the results very complete, still.

;; ;TODO: include and print with the other results from TPB.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *base-url* "http://katcr.co")

(defparameter *search-url* "https://katcr.co/new/search-torrents.php?search={}&sort=seeders&order=desc"
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
  (parse-integer (elt (lquery:$ node ".ttable_col2" (text)) 1)))

(defun torrents (words &key (stream t))
  "Return a list of..."
  (format stream "searching on Kat…")
  (let* ((query (join-for-query words))
         (url (str:replace-all "{}" query *search-url*))
         (req (request url))
         (parsed (parse req))
         (results (query parsed))
    ;; (setf results (coerce results 'list))
         (toret (map 'list (lambda (node)
                 ;; With hash-table: ok but lacks pretty printing. Again a detail.
                 ;; (let (atorrent)
                 ;;   (setf atorrent (make-hash-table :test #'equalp))
                 ;;   (setf (gethash :title atorrent)
                 ;;         (lquery:$ node ".cellMainLink" (text)))
                 ;;   (setf (gethash :href atorrent)
                 ;;         (lquery:$ node ".cellMainLink" (attr :href)))
                 ;;   ))
                             `((:title . ,(result-title node))
                               (:href . ,(str:concat *base-url* "/new/" (result-href node)))
                               (:seeders . ,(result-seeders node))
                               (:source . :kat))
                             )
                     results)))
    (format stream " found ~a results." (length toret))
    toret))

(defun find-magnet-link (parsed)
  "parsed: plump node."
  (let* ((hrefs (coerce (lquery:$ parsed "a" (attr :href)) 'list))
         (magnet (remove-if-not (lambda (it)
                                  (str:starts-with? "magnet" it))
                                hrefs)))
    (first magnet)))
