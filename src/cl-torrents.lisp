(in-package :cl-user)
(defpackage cl-torrents
  (:use :cl))
;; to do: shadow-import to use search as a funnction name.
(in-package :cl-torrents)


(defparameter *search-url* "https://piratebay.to/search/?FilterStr={KEYWORDS}&ID=&Limit=800&Letter=&Sorting=DSeeder"
  "Base search url. KEYWORDS to be replaced by the search terms (a string with +-separated words).")

(defparameter *selectors* ".Title")

(defun torrents (words)
  "Search torrents."
  (let* ((terms (str:words words))
         (query (str:join "+" terms))
         (*search-url* (str:replace-all "{KEYWORDS}" query *search-url*))
         (req (dex:get *search-url*))
         (html (plump:parse req))
         (res (lquery:$ html *selectors* (text))))
    res))
