(in-package :cl-user)
(defpackage tpb
  (:use :cl)
  (:import-from :cl-torrents.utils
                :colorize-all-keywords
                :keyword-color-pairs
                :exit
                :sublist)
  (:export :torrents
           :find-magnet-link
           :magnet))
;; to do: shadow-import to use search as a funnction name.
(in-package :tpb)

(defparameter *search-url* "https://piratebay.to/search/?FilterStr={KEYWORDS}&ID=&Limit=800&Letter=&Sorting=DSeeder"
  "Base search url. KEYWORDS to be replaced by the search terms (a string with +-separated words).")

(defparameter *selectors* "tbody tr")

(defparameter *prefilter-selector* "tbody" "Call before we extract the search results.")



(defun request (url)
  "Wrapper around dex:get. Fetch an url."
  (dex:get url))

(defun torrents (words &key (stream t))
  "Search torrents."
  (format stream "searching on the Pirate Bay…")
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
                             `((:title . ,(result-title node))
                               (:href . ,(result-href node))
                               (:leechers . ,(result-leechers node))
                               (:seeders . ,(result-peers node))
                               (:source . :tpb)))
                     res)))
    (format stream " found ~a results.~&" (length res))
    toret))

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

(defun find-magnet-link (parsed)
  "Extract the magnet link. `parsed': plump:parse result."
  (let* ((hrefs (coerce (lquery:$ parsed "a" (attr :href)) 'list))
         (magnet (remove-if-not (lambda (it)
                                  (str:starts-with? "magnet" it))
                                hrefs)))
    (first magnet)))
