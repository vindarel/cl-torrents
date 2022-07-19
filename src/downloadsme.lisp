(in-package :cl-user)
(defpackage torrents.downloadsme
  (:use :cl)
  (:import-from :torrents.models
                :make-torrent)
  (:import-from :torrents.utils
                :parse-size
                :join-for-query
                :sublist)
  (:export :torrents)
  )
(in-package :torrents.downloadsme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scraping torrentdownloads.me
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *base-url* "https://www.torrentdownloads.me")

(defparameter *source* :downloadsme
  "Source short name for human presentation.")

(defparameter *search-url* "https://www.torrentdownloads.me/search/?new=1&s_cat=0&search={}"
  "Base url for a search. Sorted by seeders")
  ;; &srt=seeds&pp=50&order=desc doesn't sort anything.

(defparameter *results-selector* "" "CSS selector to get a list of items inside the search results.")
(setf *results-selector* ".inner_container .grey_bar3")


(defparameter *search-results* nil
  "List of the last search results (plump nodes) (to eas e2e tests).")

(defun request (url)
  (dex:get url
           :headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18"))))

(defun parse (html)
  ;TODO: we can use lquery:$ (initialize <request>) instead of calling plump:parse directly.
  (plump:parse html))

(defun query (parsed)
  ;; Get rid of
  ;; - the first 3 results of "fast search results",
  ;; - the 2 table headers,
  ;; - the 1st one (with most seeders) is a redirection.
  ;; Didn't find a particular class to distinguish them.
  (subseq (coerce (lquery:$ parsed *results-selector*) 'list)
          6))

(defun result-title (node)
  (handler-case
      (let ((res (elt (lquery:$ node "p" (text)) 0)))
        res)
    (error (c)
      (format *error-output* "DOWNLOADSME error parsing title: ~a" c)
      "")))

(defun result-href (node)
  (handler-case
      (elt (lquery:$ node "p a" (attr :href)) 0)
    (error ()
      (format *error-output* "DOWNLOADSME error parsing href")
      "")))

(defun result-seeders (node)
  ;; could be a better selector.
  (handler-case
      (parse-integer (elt (lquery:$ node "span" (text)) 2))
    (error ()
      -1)))

(defun result-leechers (node)
  ;; could be a better selector.
  (handler-case
      (parse-integer (elt (lquery:$ node "span" (text)) 1))
    (error ()
      -1)))

(defun result-size (node)
  (handler-case
      (parse-size (elt (lquery:$ node "span" (text)) 3))
    (error ()
      -1)))

;TODO: refactor with other scrapers.
(defun torrents (words &key (stream t))
  "Return a list of..."
  (format stream "searching '~a' on ~a..." (str:join " " words)
          (cl-ansi-text:cyan (string *source*)))
  (handler-case
      (let* ((terms (if (listp words)
                        words
                        ;; The main gives words as a list,
                        ;; the user at the Slime REPL one string.
                        (str:words words)))
             (query (str:join "+" terms))
             (url (str:replace-all "{}" query *search-url*))
             (req (request url))
             (parsed (parse req))
             (results (query parsed))
             ;; (setf results (coerce results 'list))
             (toret (map 'list (lambda (node)
                                 (let ((title (result-title node)))
                                   (when (not (str:blank? title))
                                     (multiple-value-bind (size size-unit)
                                         (result-size node)
                                       (make-torrent
                                        :title title
                                        :href (str:concat *base-url* (result-href node))
                                        :seeders (result-seeders node)
                                        :leechers (result-leechers node)
                                        :size size
                                        :size-unit size-unit
                                        :source *source*)))))
                         results))
             ;; It has a pb with the parsing of node of index 0.
             ;; It's simpler to filter out nil objects than to fix the scraper...
             (toret (remove-if #'null toret)))
        (format stream " found ~a results.~&" (length toret))
        (setf *search-results* toret)
        toret)
    (usocket:connection-refused-error ()
      (uiop:format! *error-output* "~&error searching on ~a: ~a"
                    (cl-ansi-text:cyan (string *source*))
                    (cl-ansi-text:red "the site is unreachable")))
    (error (c)
      (uiop:format! stream " no results.~&")
      ;; xxx: logging
      (format *error-output* "error searching on ~a: ~a~&" *source* c))))
