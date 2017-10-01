(in-package :cl-user)
(defpackage cl-torrents.utils
  (:use :cl)
  (:export :sublist))

(in-package :cl-torrents.utils)


(defun sublist (l start end)
  "Select a sublist when end can be superior to the size of the
list. Wrapper around subseq that fails with 'bouncing indices bad
error'."
  ;; PR to fix this in CL21.
  (subseq l start (if (> end (length l))
                      (length l)
                      end)))
