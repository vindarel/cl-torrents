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
                :download)
  (:export :search
           :browse
           :download
           :magnet
           :url))

(in-package :torrents.commands)

(defun search (search &rest words)
  "Search for torrents on the different sources and print the results, sorted by number of seeders."
  (setf search (cons search words))
  (search-torrents search))

(defun magnet (index)
  (format t "~a~t" (torrents:magnet index)))

(defun url (index)
  (format t "~a~t" (torrents:url index)))
