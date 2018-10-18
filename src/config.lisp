
(in-package :torrents)

(defvar *cfg* (py-configparser:make-config)
  "The config read from the config.conf files found, in order: in this
  project root, ~/.config/cl-torrents.conf, in the current directory.")

(defvar *cfg-sources* nil)


(defun config-has-scrapers-option-p (cfg)
  ;; ignore "no section" errors.
  (ignore-errors
    (py-configparser:has-option-p cfg "default" "scrapers")))

(defun config-scrapers ()
  "Return the list of torrent functions to search with."
  (let* ((scrapers (replic.config:option "scrapers"))
         (scrapers (str:words (str:replace-all "," " " scrapers)))
         (torrents-list))
    (loop for it in scrapers
       do (when (assoc-value *scrapers-alist* it :test #'equal)
            (push (assoc-value *scrapers-alist* it :test #'equal) torrents-list)))

    (unless *torrents-list*
      (format t "info: looks like you disabled all torrent sources.~&")
      (finish-output))
    torrents-list))
