
build:
	#TODO:
	sbcl --load cl-torrents.asd \
	     --eval '(ql:quickload :cl-torrents)' \
	     --eval '(use-package :cl-torrents)' \
             --eval "(sb-ext:save-lisp-and-die #p\"torrents\" :toplevel #'main :executable t)"

test:
	sbcl --load cl-torrents.asd \
	     --eval '(ql:quickload :cl-torrents)' \
	     --eval '(prove:run #P"t/cl-torrents.lisp")' \
	     --eval '(quit)'

test-end2end:
	sbcl --load cl-torrents.asd \
	     --eval '(ql:quickload :cl-torrents)' \
	     --eval '(prove:run #P"t/end2end.lisp")' \
	     --eval '(quit)'
