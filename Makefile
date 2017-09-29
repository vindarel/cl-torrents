
build:
	#TODO:
	sbcl --load cl-torrents.asd \
	     --eval '(ql:quickload :cl-torrents)' \
	     --eval '(use-package :cl-torrents)' \
             --eval "(sb-ext:save-lisp-and-die #p\"torrents\" :toplevel #'main :executable t)"
