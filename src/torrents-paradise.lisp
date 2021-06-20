(in-package :cl-user)
(defpackage torrents.torrents-paradise
  (:use :cl)
  (:import-from :torrents.models
                :make-torrent)
  (:import-from :access
                :access)
  (:import-from :torrents.utils
                :colorize-all-keywords
                :keyword-color-pairs
                :exit
                :sublist)
  (:export :torrents)
  (:documentation "Search on torrents-paradise API."))

(in-package :torrents.torrents-paradise)

#|

https://torrent-paradise.ml/api/search?q=matrix

Results seem to be sorted by seeders.

Results are a list of this:

 {
    "id": "c7c2c0f0082a26409170cf2d437ef3c941f40906",  <-- base of magnet link
    "text": "The.Matrix.Reloaded.ReTaiL.2003.BDRip.XviD.DuaL-PpB",
    "len": 2368649977,
    "s": 0,
    "l": 0
  },

|#                                        ;
                                        ;
(defparameter *source-name* "torrents-paradise" ;
"Human readable name of this source.")  ;

(defparameter *source-name-short* "paradise"
  "Short name to display data, for example in a 80-chars width window.")

(defparameter *search-url* "https://torrent-paradise.ml/api/search?q={KEYWORDS}"
  "Base search API end point. KEYWORDS to be replaced by the search terms (a string with +-separated words).")

(defparameter *max-results* 20
  "Max number of results to return. To not clutter our output.")

(defun request (url)
  "Wrapper around dex:get. Fetch an url."
  (dex:get url))

(defun torrents (words &key (stream t))
  "Search torrents."
  (format stream "searching '~a' on ~a…" words *source-name*)
  (handler-case
      (let* ((terms (if (listp words)
                        words
                        ;; The main gives words as a list,
                        ;; the user at the Slime REPL one string.
                        (str:words words)))
             (query (str:join "+" terms))
             (search-url (str:replace-all "{KEYWORDS}" query *search-url*))
             (req (request search-url))
             (data (jojo:parse req))
             (res (subseq data 0 *max-results*))
             (toret (map 'list (lambda (node)
                                 (make-torrent
                                  :title (access node "text")
                                  :href (format nil "torrent URL not available on ~a" *source-name*)
                                  :seeders (access node "s")
                                  :leechers (access node "l")
                                  :magnet-link (get-magnet-link (access node "id"))
                                  :source :paradise))
                         res)))
        (format stream " found ~a results.~&" (length data))
        toret)
    (error ()
      (format stream " no results.~&"))))

(defun get-magnet-link (id)
  "From this ID, construct the magnet link."
  ;; See the sources
  ;; https://github.com/urbanguacamole/torrent-paradise/blob/19224e723c5b6c9a34009c8f3d75434412700231/website/resultpage/index.html
  (str:concat "magnet:?xt=urn:btih:"
              id
              "&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969%2Fannounce&tr=udp%3A%2F%2Ftracker.opentrackr.org%3A1337%2Fannounce&tr=udp%3A%2F%2Ftracker.internetwarriors.net%3A1337"))
