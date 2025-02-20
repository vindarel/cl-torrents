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
               :x.let-star
               :cl-transmission
               :access
               :dexador
               :jonathan
               :plump
               :str
               :lparallel
               :cl-ansi-text
               :unix-opts ;; with alias opts
               :clache
               :lquery
               :py-configparser
               :cl-readline
               :parse-float
               :log4cl)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "models")
                 (:file "tpb")
                 (:file "torrentcd")
                 (:file "kat")
                 (:file "1337")
                 (:file "downloadsme")
                 (:file "torrents-paradise")
                 (:file "torrents")
                 ;; (:file "transmission-remote")
                 (:file "commands")
                 (:file "config"))))
  ;; build executable with asdf:make :torrents.
  :build-operation "program-op"
  :build-pathname "torrents"
  :entry-point "torrents:main"

  :description "Search for torrents on popular trackers. Lisp library, CLI interface, terminal application, Tk GUI."
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

;; build a smaller executable with SBCL's core compression:
;; 84 to 23MB, however startup time increases from 0.04 to 0.35s (noticeable).
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem torrents/tk
  :version (:read-file-form "version.lisp-expr")
  :depends-on (:torrents
               :nodgui)
  :components ((:module "src/gui-tk"
                        :components
                        ((:file "gui-tk"))))
  :description "Simple GUI to search for torrents."
  ;; build an executable with asdf:make :torrents/tk
  :build-operation "program-op"
  :build-pathname "torrents-tk"
  :entry-point "torrents-tk:main")
