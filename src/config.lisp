(in-package :torrents)

;; All the logic of reading config files and setting parameters
;; is done in replic.config.

(defun config-scrapers ()
  "Filter the global `*scrapers-alist*` with the `scrapers` parameter in config.
   Return the list of torrent functions to search with."
  (let* ((scrapers (replic.config:option "scrapers"))
         (scrapers (str:words (str:replace-all "," " " scrapers)))
         (torrents-list))
    (loop for it in scrapers
       do (when (assoc-value *scrapers-alist* it :test #'equal)
            (push (assoc-value *scrapers-alist* it :test #'equal) torrents-list)))

    (unless *torrents-list*
      (format t "Info: it looks like you disabled all torrent sources.~&")
      (finish-output))
    torrents-list))
