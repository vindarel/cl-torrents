#|
  This file is a part of cl-torrents project.
|#

(in-package :cl-user)
(defpackage torrents-asd
  (:use :cl :asdf))
(in-package :torrents-asd)

(defsystem torrents
  :version (:read-file-form "version.lisp-expr")
  :author "vindarel"
  :license "MIT"
  :depends-on (
               :replic
               :dexador
               :plump
               :str
               :lparallel
               :cl-ansi-text
               :unix-opts ;; with alias opts
               :clache
               :mockingbird
               :lquery
               :py-configparser
               :alexandria  ;; to remove tmp !
               :cl-readline)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "tpb")
                 (:file "torrentcd")
                 (:file "kat")
                 (:file "1337")
                 (:file "downloadsme")
                 (:file "torrents")
                 (:file "commands")
                 (:file "config"))))
  ;; build executable with asdf:make :torrents.
  :build-operation "program-op"
  :build-pathname "torrents"
  :entry-point "torrents:main"

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
  :in-order-to ((test-op (test-op torrents-test))))
