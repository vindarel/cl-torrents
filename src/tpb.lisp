(in-package :cl-user)
(defpackage tpb
  (:use :cl)
  (:import-from :torrents.models
                :make-torrent)
  (:import-from :torrents.utils
                :colorize-all-keywords
                :keyword-color-pairs
                :exit
                :sublist)
  (:export :torrents))
;; to do: shadow-import to use search as a funnction name.
(in-package :tpb)


;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; This piratebay clone is down.
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!


(defparameter *search-url* "http://piratebay.to/search/?FilterStr={KEYWORDS}&ID=&Limit=800&Letter=&Sorting=DSeeder"
  "Base search url. KEYWORDS to be replaced by the search terms (a string with +-separated words).")

(defparameter *selectors* "tbody tr")

(defparameter *prefilter-selector* "tbody" "Call before we extract the search results.")



(defun request (url)
  "Wrapper around dex:get. Fetch an url."
  (dex:get url))

(defun torrents (words &key (stream t))
  "Search torrents."
  (format stream "searching '~a' on ~a..." (str:join " " words)
          (cl-ansi-text:cyan "the Pirate Bay"))
  (handler-case
      (let* ((terms (if (listp words)
                        words
                        ;; The main gives words as a list,
                        ;; the user at the Slime REPL one string.
                        (str:words words)))
             (query (str:join "+" terms))
             (*search-url* (str:replace-all "{KEYWORDS}" query *search-url*))
             (req (request *search-url*))
             (html (plump:parse req))
             (res (lquery:$ html *selectors*))
             (toret (map 'list (lambda (node)
                                 (make-torrent
                                  :title (result-title node)
                                  :href  (result-href node)
                                  :seeders (result-seeders node)
                                  :leechers (result-leechers node)
                                  :source :tpb))
                         res)))
        (format stream " found ~a results.~&" (length res))
        toret)

    (usocket:connection-refused-error ()
      (uiop:format! *error-output* "~&error searching on ~a: ~a"
                    (cl-ansi-text:cyan "the Pirate Bay")
                    (cl-ansi-text:red "the site is unreachable")))
    (error ()
      (format stream " no results.~&"))))

(defun result-title (node)
  "Return the title of a search result."
  (aref
   (lquery:$ node ".Title a" (text))
   0))

(defun result-href (node)
  (let* ((href-vector (lquery:$ node "a" (attr :href))))
    (aref href-vector 0)))

(defun result-peers-or-leechers (node index)
  "Return the number of peers (int) of a search result (node: a plump node).
index 0 => peers, index 1 => leechers."
  (let ((res (aref (lquery:$ node ".Seeder .ColorC" (text)) ;; returns seeders and leechers.
                   index)))
    (parse-integer res)))

(defun result-peers (node)
  (result-peers-or-leechers node 0))

(defun result-leechers (node)
  (result-peers-or-leechers node 1))
