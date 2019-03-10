(in-package :cl-user)

;; Commands to give to replic.
;;
;; We want a "search" command, which conflicts with cl:search,
;; and we want to print results on stdout.

(defpackage torrents.commands
  (:use :cl)
  (:shadow :search)
  (:import-from :torrents
                :search-torrents
                :browse
                :download
                :filter)
  ;; Some functions already print their output, we just import & re-export them.
  ;; see https://github.com/takagi/cl-reexport/
  (:export :search
           :browse
           :download
           :magnet
           :url
           :filter))

(in-package :torrents.commands)

;; All commands can complete the result ids.
;; Some commands will reduce the completion list and make it more practical.
(setf replic.completion:*default-command-completion* (lambda ()
                                                       ;; Inside a lambda because
                                                       ;; the list changes ;)
                                                       torrents::*ids-completion-list*))

(defun search (search &rest words)
  "Search for torrents on the different sources and print the results, sorted by number of seeders."
  (setf search (cons search words))
  (search-torrents search))

(defun magnet (index)
  (format t "~a~&" (torrents:magnet index)))

(defun url (index)
  (format t "~a~&" (torrents:url index)))
