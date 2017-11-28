(in-package :cl-user)
(defpackage torrents-test.end2end
  (:use :cl
        :cl-torrents
        :prove))
(in-package :torrents-test.end2end)

;;
;; These tests do network access: end-to-end tests.
;;

(plan nil)

;; prints the search results, may be nice to check output.
(ok (torrents "matrix") "The search to the Pirate Bay.to works without problems.")

;; (ok
;;  (with-output-to-string (out)
;;    (torrents "matrix" :stream out))
;;  "The search to the Pirate Bay.to works without problems.")

(finalize)
