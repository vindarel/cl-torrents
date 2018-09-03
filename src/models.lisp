(in-package :cl-user)
(defpackage torrents.models
  (:use :cl)

  (:export :make-torrent
           :title
           :href
           :seeders
           :leechers
           :source))
(in-package :torrents.models)

(defclass torrent ()
  ((title
    :initarg :title
    :initform ""
    :accessor title)
   (href
    :initarg :href
    :accessor href)
   (seeders
    :initarg :seeders
    :accessor seeders)
   (leechers
    :initarg :leechers
    :accessor leechers)
   (source
    :initarg :source
    :accessor source)
   ;; (magnet
   ;;  :initarg :magnet
   ;;  :accessor magnet)
   ))

(defmethod print-object ((it torrent) stream)
  (print-unreadable-object (it stream :type t)
    (format stream "~a, ~a"
            (str:prune 30 (if (slot-boundp it 'title)
                              (title it)
                              ""))
            (source it))))

(defun make-torrent (&key title href seeders leechers source)
  (assert title)
  (assert source)
  (assert href)
  (make-instance 'torrent
                 :title title
                 :href href
                 :seeders seeders
                 :leechers leechers
                 :source source))
