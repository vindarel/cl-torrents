(in-package :cl-user)
(defpackage torrents.models
  (:use :cl)

  (:export :make-torrent
           :title
           :href
           :seeders
           :leechers
           :size
           :size-unit
           :format-size
           :magnet-link
           :source))
(in-package :torrents.models)

(defclass torrent ()
  ((title
    :initarg :title :initform ""
    :accessor title)
   (href
    :initarg :href :initform ""
    :accessor href)
   (seeders
    :initarg :seeders :initform nil
    :accessor seeders)
   (leechers
    :initarg :leechers :initform nil
    :accessor leechers)
   (size
    :initarg :size :initform nil
    :accessor size)
   (size-unit
    :initarg :size-unit :initform nil
    :accessor size-unit)
   (magnet-link
    :initarg :magnet-link :initform nil
    :accessor magnet-link)
   (source
    :initarg :source :initform nil
    :accessor source)
   ;; (magnet
   ;;  :initarg :magnet
   ;;  :accessor magnet)
   ))

(defmethod size ((it torrent))
  "Print this torrent's size.
   If the slot is unbound (old cache results), log and return nil."
  (if (slot-boundp it 'size)
      (slot-value it 'size)
      (progn
        (log:info "the size slot of this result is unbound. You might want to delete the cache directory and try again.")
        nil)))

(defmethod size-unit ((it torrent))
  "Deal with older cache results for which the size-unit slot is unbound."
  (if (slot-boundp it 'size-unit)
      (slot-value it 'size-unit)
      ""))

(defun format-size (torrent &optional (stream nil))
  "Print the size humanly, with its unit."
  (format stream "~8@a" (format nil "~3,2f ~a" (size torrent) (size-unit torrent))))

(defmethod print-object ((it torrent) stream)
  (print-unreadable-object (it stream :type t)
    (format stream "~a, x~a seeders, ~a"
            (str:prune 30 (if (slot-boundp it 'title)
                              (title it)
                              ""))
            (seeders it)
            (source it))))

(defun make-torrent (&key title href seeders leechers size source size-unit
                       magnet-link)
  (assert title)
  (assert source)
  (assert href)
  (make-instance 'torrent
                 :title title
                 :href href
                 :seeders seeders
                 :leechers leechers
                 :size size
                 :size-unit size-unit
                 :magnet-link magnet-link
                 :source source))
