(in-package :cl-user)
(defpackage torrents
  (:use :cl
        :clache)
  (:import-from :torrents.utils
                :colorize-all-keywords
                :keyword-color-pairs
                :exit
                :arg-parser-failed
                :unknown-option
                :missing-arg
                :when-option
                :find-magnet-link
                :sublist)
  (:export :torrentsearch
           :async-torrents
           :magnet
           :main))
(in-package :torrents)

(defparameter *version* (uiop:read-file-form (asdf:system-relative-pathname :torrents #p"version.lisp-expr")))

(defparameter *last-search* nil "Remembering the last search.")
(defparameter *nb-results* 20 "Maximum number of search results to display.")
(defparameter *keywords* '() "List of keywords given as input by the user.")
(defvar *keywords-colors* nil
  "alist associating a keyword with a color. See `keyword-color-pairs'.")

(defparameter *config-directory* (merge-pathnames #p".cl-torrents/" (user-homedir-pathname))
        "The directory to put configuration files.")

(defparameter *cache-directory*
  (merge-pathnames #p"cache/" *config-directory*)
  "The directory where cl-torrents stores its cache.")

(defun ensure-cache ()
  (ensure-directories-exist *cache-directory*))

(defparameter *store* (progn
                        (ensure-cache)
                        (make-instance 'file-store :directory *cache-directory*))
  "Cache. The directory must exist.")

(defun assoc-value (alist key &key (test #'equalp))
  ;; Don't import Alexandria just for that.
  ;; See also Quickutil to import only the utility we need.
  ;; http://quickutil.org/lists/
  (cdr (assoc key alist :test test)))

(defun save-results (terms val &key (store *store*))
  "Save results in cache."
  (when val
    (setcache terms val store)))

(defun get-cached-results (terms &key (store *store*))
  (when (getcache terms store)
    (progn
      ;; (format t "Got cached results for ~a.~&" terms)
      (getcache terms store))))

(defun torrentsearch (words &key (stream t) (nb-results *nb-results*) (log-stream t))
  "Search for torrents on the different sources and print the results, sorted by number of seeders.
`words': a string (space-separated keywords) or a list of strings.
`nb-results': max number of results to print.
`log-stream': used in tests to capture (and ignore) some output."
  (let ((res (async-torrents words :stream stream :log-stream log-stream)))
    (display-results :results res :stream stream :nb-results nb-results)))

(defun async-torrents (words &key (stream t) (log-stream t))
  "Call the scrapers in parallel and sort by seeders."
  ;; With mapcar, we get a list of results. With mapcan, the results are concatenated.
  (let* ((terms (if (listp words)
                    ;; The main function gives words as a list,
                    ;; the user at the REPL a string.
                    words
                    (str:words words)))
         (joined-terms (str:join "+" terms))
         (cached-res (get-cached-results joined-terms))
         (res (if cached-res
                  ;; the cache is mixed with "torrents" and "async-torrents": ok.
                  cached-res
                  (mapcan (lambda (fun)
                            (lparallel:pfuncall fun terms :stream log-stream))
                          '(
                            ;; tpb:torrents
                            ;; kat:torrents
                            torrents.1337:torrents
                            ;; torrentcd:torrents
                            ))))
         (sorted (sort res (lambda (a b)
                             ;; maybe a quicker way, to just give the key ?
                             (> (assoc-value a :seeders)
                                (assoc-value b :seeders))))))
    (setf *keywords* terms)
    (setf *keywords-colors* (keyword-color-pairs terms))
    (setf *last-search* sorted)
    (unless cached-res
      (save-results joined-terms sorted))
    sorted))

(defun display-results (&key (results *last-search*) (stream t) (nb-results *nb-results*) (infos nil))
  "Results: list of plump nodes. We want to print a numbered list with the needed information (torrent title, the number of seeders,... Print at most *nb-results*."
  (mapcar (lambda (it)
            ;; I want to color the output.
            ;; Adding color characters for the terminal augments the string length.
            ;; We want a string padding for the title of 65 chars.
            ;; We must add to the padding the length of the extra color markers,
            ;; thus we must compute it and format the format string before printing the title.
            ;;
            ;; xxx see also the v directive: https://stackoverflow.com/questions/48868555/in-common-lisp-format-how-does-recursive-formatting-work
            (let* ((title (assoc-value it :title))
                   (title-colored (colorize-all-keywords title *keywords-colors*))
                   (title-padding (+ 65
                                     (- (length title-colored)
                                        (length title))))
                   ;; ~~ prints a ~ so here ~~~aa with title-padding gives ~65a or ~75a.
                   (format-string (format nil "~~3@a: ~~~aa ~~4@a/~~4@a ~~a~~%" title-padding)))

              (format stream format-string
                    (position it results)
                    title-colored
                    (assoc-value it :seeders)
                    (assoc-value it :leechers)
                    (assoc-value it :source)
                    )
              (if infos
                  (format stream "~a~&" (assoc-value it :href)))))
          (reverse (sublist results 0 nb-results)))
  t)

(defun request-details (url)
  "Get the html page of the given url. Mocked in unit tests."
  (dex:get url))

(defun magnet-link-from (alist)
  "Extract the magnet link from a `torrent' result.

   Return the first href of the page that starts with 'magnet'."
  (let* ((url (assoc-value alist :href))
         (html (request-details url))
         (parsed (plump:parse html)))
    (find-magnet-link parsed)))

(defun magnet (index)
  "Search the magnet from last search's `index''s result."
  (if *last-search*
      (if (< index (length *last-search*))
          (magnet-link-from (elt *last-search* index))
          (format t "The search returned ~a results, we can not access the magnet link n°~a.~&" (length *last-search*) index))
      (format t "no search results to get the magnet link from.~&")))

(defun url-from (index)
  "Return the url from last search's `index''s result."
  (if *last-search*
      (if (< index (length *last-search*))
          (assoc-value (elt *last-search* index) :href)
          (format t "index too big, the last search only returned ~a results.~&" (length *last-search*)))
      (format t "no search results to get the url from.~&")))

(defparameter *commands* '(
                           ("search" . "<keywords> search torrents")
                           ("magnet" . "<i> get magnet link from search result nb i") ;; with arg
                           ("details" . "toggle the display of details")
                           ("nb-results" . "<n> set the number of results to print")
                           ;; "nb"
                           ;; ("info" . "print a recap")
                           ("url" . "<i> get the torrent's page url")
                           ("open" . "<i> open this torrent page with the default browser")
                           ("firefox" . "<i> open this torrent page with firefox")
                           ("version" . "print cl-torrents version")
                           ("help" . "print this help")
                           ("quit" . "quit")
                           )
  "List of alist tuples, a verb and its doc, for completion on the REPL.")

(defparameter *verbs* (mapcar #'first *commands*)
  "List of verbs for completion. Strings.")

(defun common-prefix (items)
  ;; tmp waiting for cl-str 0.5 in Quicklisp february.
  "Find the common prefix between strings.

   Uses the built-in `mismatch', that returns the position at which
   the strings fail to match.

   Example: `(str:common-prefix '(\"foobar\" \"foozz\"))` => \"foo\"

   - items: list of strings
   - Return: a string.

  "
  ;; thanks koji-kojiro/cl-repl
  (when items (subseq
               (car items)
               0
               (apply
                #'min
                (mapcar
                 #'(lambda (i) (or (mismatch (car items) i) (length i)))
                 (cdr items))))))

(defun select-completions (text list)
  "Select all verbs from `list' that start with `text'."
  (let ((els (remove-if-not (alexandria:curry #'str:starts-with? text)
                            list)))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))

(defun custom-complete (text start end)
  "Complete a symbol when the cursor is at the beginning of the prompt."
  (declare (ignore end))
  (if (zerop start)
      (select-completions text *verbs*)))

(defun repl-help ()
  (mapcar (lambda (it)
                    (format t "~10a-~t ~a~&" (car it) (cdr it)))
          *commands*))

(defparameter *browser* "firefox"
  "Default browser, in case $BROWSER is not set.")

(defun browse (index)
    "Open with the default browser (or firefox). Use from Slime."
    (let ((url (url-from index))
          (browser (or (uiop:getenv "BROWSER")
                       *browser*)))
      (declare (ignorable browser))
      (if url
          (uiop:launch-program (list browser
                                     url))
          (format t "couldn't find the url of ~a, got ~a~&" index url))))

(defun open-with-browser (args)
  "Open firefox to this search result's url. Use from the repl."
  (let* ((index (parse-integer (first args))))
    (browse index)))

(defun repl ()
  "Start a readline interactive prompt.

  Auto completes some verbs (search, magnet, details,...).

  "

  (rl:register-function :complete #'custom-complete)

  (handler-case
      (do ((i 0 (1+ i))
           (text "")
           (verb "")
           (args "")
           (details nil))
          ((string= "quit" (str:trim text)))
        (setf text
              (rl:readline :prompt (cl-ansi-text:green (format nil "cl-torrents [~a] > " i))
                           :add-history t))
        (setf verb (first (str:words text)))
        (setf args (rest (str:words text)))

        (cond
          ((or (string= "fortune" verb)
               (string= "yo" verb))
           (if (probe-file "/usr/games/fortune")
               (uiop:run-program "/usr/games/fortune" :output *standard-output*)
               (format t "nothing in /usr/games/fortune, man.~&")))

          ((string= "version" verb)
           (format t "~a~&" *version*))

          ((or (string= "help" verb)
               (string= "?" verb))
           (repl-help))

          ((string= "details" verb)
           (setf details (not details))
           (format t "details set to ~a~&" details))

          ((string= "nb-results" verb)
           (setf *nb-results* (parse-integer (first args))))

          ((string= "search" verb)
           (format t "searching: ~a~&" args)
           ;; catch errors (network)
           (display-results :results (async-torrents args)
                            :nb-results *nb-results*
                            :infos details))

          ((string= "magnet" verb)
           (format t "~a~&" (magnet (parse-integer (first args)))))

          ((string= "url" verb)
           (format t "~a~&" (url-from (parse-integer (car args)))))

          ((or (string= "open" verb)
               (string= "firefox" verb))
           (open-with-browser args)))

          (finish-output))

    (#+sbcl sb-sys:interactive-interrupt
      () (progn
           (uiop:quit)))
    (error (c)
      (format t "Unknown error: ~&~a~&" c))))

(defun main ()
  "Parse command line arguments (portable way) and call the program."

  ;; if not inside a function, can not build an executable (can not
  ;; save core with multiple threads running).
  (setf lparallel:*kernel* (lparallel:make-kernel 2))

  (ensure-cache)

  ;; Define the cli args.
  (opts:define-opts
    (:name :help
           :description "print this help text"
           :short #\h
           :long "help")
    (:name :version
           :description "print the version"
           :short #\v
           :long "version")
    (:name :nb-results
           :description "maximum number of results to print."
           :short #\n
           :long "nb"
           :arg-parser #'parse-integer)
    (:name :details
           :description "print more details (like the torrent's url)"
           :short #\d
           :long "details")
    (:name :magnet
           :description "get the magnet link of the given search result."
           :short #\m
           :long "magnet"
           :arg-parser #'parse-integer)
    (:name :interactive
           :description "enter an interactive repl"
           :short #\i
           :long "interactive"))


  (multiple-value-bind (options free-args)
      ;; opts:get-opts returns the list of options, as parsed,
      ;; and the remaining free args as second value.
      (handler-bind ((opts:unknown-option #'unknown-option)
                     (opts:missing-arg #'missing-arg)
                     (opts:arg-parser-failed #'arg-parser-failed))
                     ;; (opts:missing-required-option) ;; => upcoming version
        (opts:get-opts))

    (if (getf options :help)
        (progn
          (opts:describe
           :prefix (format nil "CL-torrents version ~a. Usage:" *version*)
           :args "[keywords]")
          (exit)))
    (if (getf options :version)
        (progn
          (format t "cl-torrents version ~a~&" *version*)
          (exit)))
    (if (getf options :nb-results)
        (setf *nb-results* (getf options :nb-results)))

    (if (getf options :interactive)
        (progn
          (repl)
          (uiop:quit)))

    ;; This is the only way I found
    ;; https://github.com/fukamachi/clack/blob/master/src/clack.lisp
    ;; trivial-signal didn't work (see issue #3)
    (handler-case
        (display-results :results (async-torrents free-args)
                         :nb-results *nb-results*
                         :infos (getf options :infos))
      (#+sbcl sb-sys:interactive-interrupt
        #+ccl  ccl:interrupt-signal-condition
        #+clisp system::simple-interrupt-condition
        #+ecl ext:interactive-interrupt
        #+allegro excl:interrupt-signal
        () (progn
             (format *error-output* "Aborting.~&")
             (exit)))
      (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))

    (if (getf options :magnet)
        (progn
          (format t "~a~&" (magnet (getf options :magnet)))
          (exit)))))
