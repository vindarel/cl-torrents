(in-package :cl-user)

;; quickload nodgui before this.
(defpackage torrents-tk
  (:use :cl
        :nodgui)
  (:export :main))

(in-package :torrents-tk)

(defun search-tree ()
  ;; not resizable :S
(defun search-tree (&optional search)
  ;; The bottom is not resizable :S
  (with-nodgui ()
    (wm-title *tk* "Torrents GUI")
    (let* ((tree (make-instance 'scrolled-treeview
                                ;; These are the second and third columns.
                                :columns (list "seeders"
                                               "leechers"
                                               "size"
                                               "source")
                                :command (lambda (selection)
                                           (log:info selection))))
           (searchbox (grid (make-instance 'entry :width 7)
                            0 0 :sticky "we" :padx 5 :pady 5))
           (button (make-instance 'button
                                  :text "OK"
                                  :command (lambda ()
                                             (format t "the treeview selection is: ~a~&"
                                                     (treeview-get-selection tree))
                                             (format t "text is: ~a~&" (text searchbox))
                                             ;; There is an error with "latin capital letters"
                                             ;; when searching "tears of steel".
                                             (insert-results tree
                                                             (torrents:search-torrents (text searchbox)))))))

      ;; Name the first column:
      (treeview-heading tree +treeview-first-column-id+ :text "name")
      ;; For resizing to do something: weight > 0
      (grid-columnconfigure *tk* 0 :weight 1)

      (grid searchbox 0 0
            :sticky "ew")
      (grid button 0 1
            ;; stick to the right (east).
            :sticky "e")
      (grid tree 1 0
            ;; so the button doesn't have a column by itself.
            :columnspan 2
            ;; sticky by all sides, for resizing to do something.
            :sticky "nsew"))))
      ;; for debugging.
      (when search
        (insert-results tree (torrents:search-torrents search))))))

(defun insert-results (tree results)
  "Insert torrents last results into that treeview."
  ;; Clear content.
  ;; this needs nodgui newer than feb, 24th 2019
  ;; with commit c9ae0ec389.
  (treeview-delete-all tree)
  (loop for result in results
     do (treeview-insert-item tree
                              :text (torrents:title result)
                              ;; xxx: numbers stick to the left instead of the right.
                              :column-values (list (torrents:seeders result)
                                                   (torrents:leechers result)
                                                   (torrents.models:format-size result)
                                                   (torrents:source result)))))

(defun main ()
  (search-tree))
