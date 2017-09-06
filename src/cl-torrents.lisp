(in-package :cl-user)
(defpackage cl-torrents
  (:use :cl)
  (:export :torrents))
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
         (res (lquery:$ html *selectors*)))
    (coerce res 'list)))

(defun detail-page-url (node)
  "Extract the link of the details page. `node': plump node, containing the url."
  (let* ((href-vector (lquery-funcs:attr (lquery:$ node "a") "href"))
         (href (aref href-vector 0)))
    href))

;; test:
;; (mapcar #'detail-page-url (torrents "matrix"))

(defun magnet-link (node)
  "Extract the magnet link."
  (let* ((hrefs (mapcar (lambda (it)
                          (lquery-funcs:attr it "href"))
                        (coerce (lquery:$ node "a") 'list)))
         (magnet (remove-if-not (lambda (it)
                                  (str:starts-with? "magnet" it))
                                hrefs)))
    (first magnet)))
