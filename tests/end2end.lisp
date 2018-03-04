(in-package :cl-user)
(defpackage torrents-test.end2end
  (:use :cl
        :torrents
        :prove))
(in-package :torrents-test.end2end)

;;
;; These tests do network access: end-to-end tests.
;;

(plan nil)

(subtest "Testing that 1337x.to responds"
  (let ((res (torrents.1337:torrents '("matrix"))))
    (ok res
        "A search is not null")

    ;; (ok (> 10 (length res))
    ;;     "10 results")

    (ok (print (first res)))

    (ok (str:starts-with? "magnet"
                          (torrents::magnet-link-from (first torrents.1337::*search-results*)))
        "We get the magnet link of the first result.")))

(finalize)
