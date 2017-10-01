(in-package :cl-user)
(defpackage cl-torrents
  (:use :cl
        :cl-torrents.utils)
  (:export :torrents
           :magnet
           :main))
;; to do: shadow-import to use search as a funnction name.
(in-package :cl-torrents)


(defparameter *search-url* "https://piratebay.to/search/?FilterStr={KEYWORDS}&ID=&Limit=800&Letter=&Sorting=DSeeder"
  "Base search url. KEYWORDS to be replaced by the search terms (a string with +-separated words).")

(defparameter *selectors* "tbody tr")

(defparameter *prefilter-selector* "tbody" "Call before we extract the search results.")

(defparameter *last-search* nil "Remembering the last search (should be an hash-map).")

(defparameter *keywords* '() "List of keywords given as input by the user.")

(defparameter *nb-results* 50 "Maximum number of search results to display.")

(defparameter *colors* '(
                         cl-ansi-text:blue
                         cl-ansi-text:green
                         cl-ansi-text:yellow
                         cl-ansi-text:cyan
                         cl-ansi-text:magenta
                         cl-ansi-text:red
                         )
  "Functions to colorize text.")

(defvar *keywords-colors* nil
  "alist associating a keyword with a color. See `keyword-color-pairs'.")

(defun request (url)
  "Wrapper around dex:get. Fetch an url."
  (dex:get url))

(defun torrents (words &optional (stream t))
  "Search torrents."
  (let* ((terms (str:words words))
         (query (str:join "+" terms))
         (*search-url* (str:replace-all "{KEYWORDS}" query *search-url*))
         (req (request *search-url*))
         (html (plump:parse req))
         (res (lquery:$ html *selectors*))
         (res-list (coerce res 'list)))
    (setf *last-search* res-list)
    (setf *keywords* terms)
    (setf *keywords-colors* (keyword-color-pairs terms))
    (display-results res-list stream)))

(defun result-title (node)
  "Return the title of a search result."
  (aref
   (lquery:$ node ".Title a" (text))
   0))

(defun colorize-keyword-in-string (title keyword color-f)
  "Colorize the given keyword in the title.
Keep the letters' possible mixed up or down case.
`color-f': color function (cl-ansi-text)."
  ;; It colorizes only the first occurence of the word.
  (let ((start (search keyword (string-downcase title) :test #'equalp)))
    (if (numberp start)
      (let* ((end (+ start (length keyword)))
             (sub (subseq title start end)) ;; that way we keep original case of each letter
             (colored-sub (funcall color-f sub)))
        (str:replace-all sub colored-sub title))
      title)))

;; closure to loop over the list of available colors.
(let ((index 0))
  (defun next-color ()
    "At each call, return the next color of the list -and start over. Uses *colors*."
    (let* ((nb-colors (length *colors*))
           (color (elt *colors* index)))
      (incf index)
      (if (>= index nb-colors)
          (setf index 0))
      color))

  (defun reset-color ()
    (setf index 0))
  )

(defun keyword-color-pairs (&optional (keywords *keywords*))
  "Associate each keyword with a different color and return a list of pairs."
  (mapcar (lambda (it)
            `(,it . ,(NEXT-COLOR)))
          keywords))

(defun colorize-all-keywords (title kw-color)
  "Colorize all the user's search keywords in the given title.
`kw-color': list of pairs with a keyword and a color (function)."
  (let ((new title))
    (loop for (word . color) in kw-color
       do (progn
            (setf new (colorize-keyword-in-string new word color))))
    new)
  )

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

(defun display-results (&optional (results *last-search*) (stream t))
  "Results: list of plump nodes. We want to print a numbered list with the needed information (torrent title, the number of seeders,... Print at most *nb-results*."
  (mapcar (lambda (it)
            ;; xxx: do not rely on *last-search*.
            ;; I want to color the output.
            ;; Adding color characters for the terminal augments the string length.
            ;; We want a string padding for the title of 65 chars.
            ;; We must add to the padding the length of the extra color markers,
            ;; thus we must compute it and format the format string before printing the title.
            (let* ((title (result-title it))
                   (title-colored (colorize-all-keywords title *keywords-colors*))
                   (title-padding (+ 65
                                     (- (length title-colored)
                                        (length title))))
                   ;; ~~ prints a ~ so here ~~~aa with title-padding gives ~65a or ~75a.
                   (format-string (format nil "~~3@a: ~~~aa ~~3@a/~~3@a~~%" title-padding)))

              (format stream format-string
                    (position it *last-search*)
                    title-colored
                    (result-peers it)
                    (result-leechers it))))
          (reverse (sublist results 0 *nb-results*)))
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

(defun request-details (url)
  "Get the html page of the given url. Mocked in unit tests."
  (dex:get url))

(defun magnet-link-from (node)
  "Extract the magnet link from a `torrent' result."
  (let* ((url (detail-page-url node))
         (html (request-details url))
         (parsed (plump:parse html)))
    (find-magnet-link parsed)))

(defun magnet (index)
  "Search the magnet from last search's `index''s result."
  (magnet-link-from (elt *last-search* index)))

(defun main (&rest argv)
  (torrents "matrix"))
