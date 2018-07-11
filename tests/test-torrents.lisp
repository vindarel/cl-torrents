(in-package :cl-user)
(defpackage torrents-test
  (:use :cl
        :torrents
        :mockingbird
        :prove)
  (:export :file-to-string))
(in-package :torrents-test)


(defun file-to-string (path)
  "Return the given file as a string."
    (with-open-file (stream path
                            :external-format :utf-8)
      (let ((data (make-string (file-length stream))))
        (read-sequence data stream)
        data)))
