
(in-package :torrents)

(defvar *cfg* (py-configparser:make-config)
  "The config read from the config.conf files found, in order: in this
  project root, ~/.config/cl-torrents.conf, in the current directory.")

(defvar *cfg-sources* nil)


(defun read-config ()
  (setf *cfg-sources* (list
                       (asdf:system-relative-pathname :torrents "cl-torrents.conf")
                       ;; Setting here and not in defparameter:
                       ;; ensure this is the user's value, not where the exec was built on.
                       (merge-pathnames "./config/cl-torrents.conf" (user-homedir-pathname))
                       "cl-torrents.conf"))
  (loop for it in *cfg-sources*
     do (progn
          (when (probe-file it)
            ;; xxx verbose
            ;; (format t "reading config ~a~&" it)
            ;; read-files reads a list.
            (py-configparser:read-files *cfg* (list it)))))
  *cfg*)

(defun config-has-scrapers-option-p (cfg)
  ;; ignore "no section" errors.
  (ignore-errors
    (py-configparser:has-option-p cfg "default" "scrapers")))

(defun config-has-option-p (cfg option)
  (ignore-errors
    (py-configparser:has-option-p cfg "default" option)))

(defun config-option (cfg option)
  (py-configparser:get-option cfg "default" option))

(defun config-torrents (cfg)
  "Return the list of torrent functions to search with."
  (let* ((scrapers (py-configparser:get-option cfg "default" "scrapers"))
         (scrapers (str:words (str:replace-all "," " " scrapers)))
         (torrents-list))
    (loop for it in scrapers
       do (when (assoc-value *scrapers-alist* it :test #'equal)
            (push (assoc-value *scrapers-alist* it :test #'equal) torrents-list)))

    (unless *torrents-list*
      (format t "info: looks like you disabled all torrent sources.~&")
      (finish-output))
    torrents-list))
