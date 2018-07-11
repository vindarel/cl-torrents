(in-package :cl-user)
(defpackage torrents.utils
  (:use :cl)
  (:export :sublist
           :find-magnet-link
           :exit
           :unknown-option
           :missing-arg
           :when-option))

(in-package :torrents.utils)

(defparameter *colors* '(
                         cl-ansi-text:blue
                         cl-ansi-text:green
                         cl-ansi-text:yellow
                         cl-ansi-text:cyan
                         cl-ansi-text:magenta
                         cl-ansi-text:red
                         )
  "Functions to colorize text.")


(defun find-magnet-link (parsed)
  "Return the firts href of the page that starts with 'magnet'.

  parsed: plump node."
  ;; It works for tpb, torrentcd and kat.
  (let* ((hrefs (coerce (lquery:$ parsed "a" (attr :href)) 'list))
         (magnet (remove-if-not (lambda (it)
                                  (str:starts-with? "magnet" it))
                                hrefs)))
    (first magnet)))

(defun sublist (l start end)
  "Select a sublist when end can be superior to the size of the
list. Wrapper around subseq that fails with 'bouncing indices bad
error'."
  ;; PR to fix this in CL21.
  ;; see substring coming in cl-str: https://github.com/vindarel/cl-str#substring-start-end-s---new-in-03
  (subseq l start (if (> end (length l))
                      (length l)
                      end)))

(defun join-for-query (entry)
  "From a string (space-separated words), return a +-separated string."
  (str:join "+" (str:words entry)))

;;;
;;; Colorize keywords in strings.
;;;

;; closure to loop over the list of available colors.
(let ((index 0))
  (defun next-color ()
    "At each call, return the next color of the list -and start over. Uses *colors*."
    (let ((nb-colors (length *colors*))
          (color (elt *colors* index)))
      (incf index)
      (if (>= index nb-colors)
          (setf index 0))
      color))

  (defun reset-color ()
    (setf index 0))
  )

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

(defparameter *keywords* '() "List of keywords given as input by the user.")

(defun keyword-color-pairs (&optional (keywords *keywords*))
  "Associate each keyword with a different color and return a list of pairs."
  (mapcar (lambda (it)
            `(,it . ,(next-color)))
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

(defun exit (&optional (status 0))
  "Exit from Lisp. Return `status' (0 by default)."
  ;; PRed upstream => merged, waiting for Quicklisp november update.
  #+sbcl      (sb-ext:exit :code status)
  #+cmu       (unix:unix-exit status)
  #+ccl       (ccl:quit status)
  #+ecl       (ext:quit status)
  #+clisp     (ext:exit status)
  #+abcl      (extensions:exit :status status)
  #+allegro   (excl:exit status :quiet t)
  #+lispworks (lispworks:quit :status status))

;; Describe handlers for bad command line arguments.
(defun unknown-option (condition)
  (format t "~s option is unknown.~%" (opts:option condition))
  (opts:describe)
  (exit))

(defun missing-arg (condition)
  (format t "Bad options: ~a needs an argument.~&" (opts:option condition))
  (opts:describe)
  (exit))

(defun arg-parser-failed (condition)
  (format t "Error: could not parse ~a as argument of ~a~&."
          (opts:raw-arg condition)
          (opts:option condition))
  (opts:describe)
  (exit))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))
