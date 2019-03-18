(in-package :cl-user)

;; quickload nodgui before this.
(defpackage torrents-tk
  (:use :cl
        :nodgui)
  (:export :main))

(in-package :torrents-tk)

(defconstant +results-columns-width+ '(498 80 80 80 150)
  "Width of the treeview columns.")

(defvar *searchbar-frame* nil
  "Search bar at the top.")

(defvar *tree-frame* nil
  "The frame containing the treeview widget.")

(defvar *tree* nil
  "The tree widget with search results.")

(defvar *bottom-buttons-frame* nil
  "The frame to position the buttons.")

(defun searchbar ()
  (let* ((frame (make-instance 'frame))
         (searchbox (make-instance 'entry
                                   :master frame
                                   :width 50))
         (button (make-instance 'button
                                :master frame
                                :text "OK"
                                :command (lambda ()
                                           (format t "the treeview selection is: ~a~&"
                                                   (treeview-get-selection *tree*))
                                           (format t "text is: ~a~&" (text searchbox))
                                           (insert-results *tree*
                                                           (torrents:search-torrents (text searchbox)))))))

    (grid searchbox 0 0
          :sticky "we"
          :padx 5 :pady 5)
    (grid button 0 1
          ;; stick to the right (east).
          :sticky "e")

    (setf *searchbar-frame* frame)))

(defun bottom-buttons ()
  "magnet, torrent, open,…"
  (let* ((frame (make-instance 'frame))
         (button-open (make-instance 'button
                                     :master frame
                                     :text "open"
                                     :command
                                     (lambda ()
                                       (let* ((selection (car (treeview-get-selection *tree*)))
                                              (items  (items *tree*))
                                              (index (position selection items))
                                              (fixed-index (when index
                                                             (- torrents:*nb-results* 1 index))))
                                         (when fixed-index
                                           (let ((url (torrents:url fixed-index)))
                                             (format t "~&opening url: ~a" url)
                                             (torrents:browse fixed-index)))))))
         (button-download (make-instance 'button
                                     :master frame
                                     :text "download"
                                     :command
                                     (lambda ()
                                       (let* ((selection (car (treeview-get-selection *tree*)))
                                              (items  (items *tree*))
                                              (index (position selection items))
                                              (fixed-index (when index
                                                             (- torrents:*nb-results* 1 index))))
                                         (when fixed-index
                                           (let ((magnet (torrents:magnet fixed-index)))
                                             (format t "~&magnet: ~a" magnet)
                                             (torrents:download fixed-index))))))))

    (setf *bottom-buttons-frame* frame)

    (grid button-open 0 1
          :sticky "e")
    (grid button-download 0 2
          :sticky "e")))

(defun search-tree (&optional search)
  ;; The bottom is not resizable :S
  (let* ((frame (make-instance 'frame))
         (tree (make-instance 'scrolled-treeview
                              :master frame
                              ;; These are the second and third columns.
                              :columns (list "seeders"
                                             "leechers"
                                             "size"
                                             "source")
                              :columns-width +results-columns-width+)))

    ;; Name the first column:
    (treeview-heading tree +treeview-first-column-id+ :text "name")

    (grid tree 0 0
          :sticky "nsew")

    (setf *tree* tree)
    (setf *tree-frame* frame)

    ;; For debugging, show some results right away.
    (when search
      (insert-results tree (torrents:search-torrents search)))))

(defun insert-results (tree results)
  "Insert torrents last results into that treeview."
  ;; Clear content.
  ;; this needs nodgui newer than feb, 24th 2019
  ;; with commit c9ae0ec389.
  (treeview-delete-all tree)
  (loop for result in (torrents.utils:sublist results 0 torrents:*nb-results*)
     do (treeview-insert-item
         tree
         :text
         (str:replace-all "~" "~~"
                          ;; Titles may contain "~J" (tears of steel), which is
                          ;; interpreted as a wrong format directive.
                          ;; This doesn't seem to occur on the repl.
                          ;; Should be fixed with commit
                          ;; https://notabug.org/cage/nodgui/commit/c178400b430eda70a574c4a0d4b4dd78f9e465a8
                          (torrents:title result))
         ;; xxx: numbers stick to the left instead of the right.
         :column-values (list (torrents:seeders result)
                              (torrents:leechers result)
                              (torrents.models:format-size result)
                              (torrents:source result)))))

(defun main (&optional search)
  (with-nodgui ()
    (wm-title *tk* "Torrents GUI")

    ;; For resizing to do something: weight must be > 0
    (grid-columnconfigure *tk* 0 :weight 1)

    (let ((row 0))
      (searchbar)
      (grid *searchbar-frame* row 0
            :sticky "w")

      (search-tree search)
      (grid *tree-frame* (incf row) 0
            :sticky "nsew")

      (bottom-buttons)
      (grid *bottom-buttons-frame* (incf row) 0
            :sticky "e"))))
