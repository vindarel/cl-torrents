(in-package :cl-user)

;; Commands to give to replic.
;;
;; We want a "search" command, which conflicts with cl:search.

(defpackage torrents.commands
  (:use :cl)
  (:shadowing-import-from :torrents
                          :search-torrents)
  (:shadow :search)
  (:import-from :torrents
                :browse
                :download
                :magnet)
  (:export :search
           :browse
           :download
           :magnet))

(in-package torrents.commands)

;; alias
(setf (fdefinition 'search) #'search-torrents)
