;; Example ~/.torrents.lisp user init file.

(in-package :torrents.user)

(defparameter *names* '()
  "List of names (string) given to `hello`. Will be autocompleted by `goodbye`.")

(defun hello (name)
  "Takes only one argument. Adds the given name to the global
  `*names*` global variable, used to complete arguments of `goodbye`. "
  (format t "hello ~a~&" name)
  (push name *names*))

(defun goodbye (name)
  "Says goodbye to name, where `name` should be completed from what was given to `hello`."
  (format t "goodbye ~a~&" name))

;; Custom completion for goodbye:
(replic.completion:add-completion "goodbye" (lambda () *names*))

;; and export the two functions to find them as commands.
(export '(hello
          goodbye))
