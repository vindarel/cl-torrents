(in-package :torrents)


(defclass transmission-remote (torrents::remote-client) ()
  (:documentation "Inherit this client from a base class, to have a
  common type to base inheritance on."))

(defun make-transmission-remote (host username password)
  "Create an instance of transmission-remote and connect.
  TODO: port is by default 9091."
  (make-instance 'transmission-remote
                 :name "transmission-remote"
                 :instance
                 (make-instance
                  'cl-transmission:transmission-connection
                  :host host
                  :credentials (list username password))))

(defmethod remote-client-connect ((client transmission-remote))
  ;TODO: useless, already connected.
  (slot-value client 'instance))

(defmethod remote-client-add ((client transmission-remote) magnet/file)
  (cl-transmission:transmission-add (instance client)
                                    :filename magnet/file))
