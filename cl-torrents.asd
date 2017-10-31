#|
  This file is a part of cl-torrents project.
|#

(in-package :cl-user)
(defpackage cl-torrents-asd
  (:use :cl :asdf))
(in-package :cl-torrents-asd)

(defsystem cl-torrents
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:str
               :dexador
               :plump
               :mockingbird
               :cl-ansi-text
               :unix-opts ;; with alias opts
               :lquery)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "cl-torrents")
                 (:file "kat"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :external-format :utf-8
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-torrents-test))))
