

(in-package :cl-user)
(defpackage torrents.rarbg
  (:use :cl)
  (:import-from :torrents.models
                :make-torrent)
  (:import-from :torrents.utils
                :parse-size
                :join-for-query
                :sublist)
  (:export :torrents)
  )
(in-package :torrents.rarbg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scraping rarbg
;;
;; WIP
;;
;; Often get this page:
;;
;; "Please wait while we try to verify your browser..."
;;
;; It has worked already.
;; Possibly make less requests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *source* :rarbg
  "Source short name for human presentation.")

(defparameter *base-url* "https://rarbgaccessed.org")

(defparameter *search-url* "https://rarbgaccessed.org/torrents.php?search={SEARCH}"
  "Base url for a search.")

;XXX: unused so far.
(defparameter *search-params*
  '((:categories . "&category%5B%5D=4&category%5B%5D=14&category%5B%5D=48&category%5B%5D=17&category%5B%5D=44&category%5B%5D=45&category%5B%5D=47&category%5B%5D=50&category%5B%5D=51&category%5B%5D=52&category%5B%5D=42&category%5B%5D=46&category%5B%5D=23&category%5B%5D=25")
    (:sort-by . "&order=seeders&by=DESC")))

(defparameter *results-selector* "" "CSS selector to get a list of items inside the search results.")
(setf *results-selector* ".lista2")


(defparameter *search-results* nil
  "List of the last search results (plump nodes) (to eas e2e tests).")

(defun request (url)
  (dex:get url
           :headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18"))))

(defun parse (html)
  (lquery:$ (initialize html)))

(defun css-query (parsed)
  (coerce (lquery:$ parsed *results-selector*) 'list))

(defun result-title (node)
  (handler-case
      (let ((res (elt (lquery:$ node ".lista" (text)) 1)))
        res)
    (error (c)
      (format *error-output* "RARBG error parsing title: ~a" c)
      "")))

(defun result-href (node)
  (handler-case
      (elt (lquery:$ node ".lista a" (attr :href)) 1)
    (error ()
      (format *error-output* "RARBG error parsing href")
      "")))

(defun result-seeders (node)
  ;; could be a better selector.
  (handler-case
      (parse-integer (elt (lquery:$ node ".lista" (text)) 4))
    (error ()
      -1)))

(defun result-leechers (node)
  ;; could be a better selector.
  (handler-case
      (parse-integer (elt (lquery:$ node ".lista" (text)) 5))
    (error ()
      -1)))

(defun result-size (node)
  (handler-case
      (parse-size (elt (lquery:$ node ".lista" (text)) 3))
    (error ()
      -1)))

(defun build-url (query)
  (when (stringp query)
    (setf query (str:words query)))
  (setf query (str:join "+" query))
  (log:debug query)
  (str:replace-all "{SEARCH}" query *search-url*))

(defun torrents (words &key (stream t))
  "Return a list of torrent objects."
  (format stream "searching '~a' on ~a... " (str:join " " words)
          (cl-ansi-text:cyan (string *source*)))
  (handler-case
      (let* ((req (request (build-url words)))
             (parsed (parse req))
             (results (css-query parsed))
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
             (toret (remove-if #'null toret)))
        (format stream " found ~a results.~&" (length toret))
        (setf *search-results* toret)
        toret)

    (usocket:connection-refused-error ()
      (uiop:format! *error-output* "~&error searching on ~a: ~a.~&"
                    *source*
                    (cl-ansi-text:red "the site is unreachable")))
    (error (c)
      (uiop:format! stream " no results.~&")
      ;; xxx: logging
      (format *error-output* "error searching on ~a: ~a~&" *source* c))))
