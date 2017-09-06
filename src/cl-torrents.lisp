(in-package :cl-user)
(defpackage cl-torrents
  (:use :cl)
  (:export :torrents
           :magnet))
;; to do: shadow-import to use search as a funnction name.
(in-package :cl-torrents)


(defparameter *search-url* "https://piratebay.to/search/?FilterStr={KEYWORDS}&ID=&Limit=800&Letter=&Sorting=DSeeder"
  "Base search url. KEYWORDS to be replaced by the search terms (a string with +-separated words).")

(defparameter *selectors* "tbody tr")

(defparameter *prefilter-selector* "tbody" "Call before we extract the search results.")

(defvar *last-search* nil "Remembering the last search (should be an hash-map).")

(defun torrents (words)
  "Search torrents."
  (let* ((terms (str:words words))
         (query (str:join "+" terms))
         (*search-url* (str:replace-all "{KEYWORDS}" query *search-url*))
         (req (dex:get *search-url*))
         (html (plump:parse req))
         (nodes (search-prefilter-results html))
         (res (lquery:$ nodes *selectors*))
         (res-list (coerce res 'list)))
    (setf *last-search* res-list)
    (display-results res-list)))


(defun result-title (node)
  "Return the title of a search result."
  (aref
   (lquery:$ node ".Title a" (text))
   0))

(defun display-results (&optional (results *last-search*))
  "Results: list of plump nodes. We want to print a numbered list with the needed information (torrent title, the number of seeders,…"
  (mapcar (lambda (it)
            (format t "~a: ~a~%" (+ 1 (position it *last-search*)) (result-title it)))
          (reverse results))
  t)

(defun detail-page-url (node)
  "Extract the link of the details page. `node': plump node, containing the url."
  (let* ((href-vector (lquery-funcs:attr (lquery:$ node "a") "href"))
         (href (aref href-vector 0)))
    href))

;; test:
;; (mapcar #'detail-page-url (torrents "matrix"))

(defun find-magnet-link (parsed)
  "Extract the magnet link. `parsed': plump:parse result."
  (let* ((hrefs (mapcar (lambda (it)
                          (lquery-funcs:attr it "href"))
                        (coerce (lquery:$ parsed "a") 'list)))
         (magnet (remove-if-not (lambda (it)
                                  (str:starts-with? "magnet" it))
                                hrefs)))
    (first magnet)))

(defun magnet-link-from (node)
  "Extract the magnet link from a `torrent' result."
  (let* ((url (detail-page-url node))
         (html (dex:get url))
         (parsed (plump:parse html)))
    (find-magnet-link parsed)))

(defun magnet (index)
  "Search the magnet from last search's `index''s result."
  (magnet-link-from (elt *last-search* index)))
