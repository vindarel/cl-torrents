
build:
	#TODO:
	sbcl --load cl-torrents.asd \
	     --eval '(ql:quickload :cl-torrents)' \
	     --eval '(use-package :cl-torrents)' \
             --eval "(sb-ext:save-lisp-and-die #p\"torrents\" :toplevel #'main :executable t)"

test:
	sbcl --load cl-torrents.asd \
	     --eval '(ql:quickload :cl-torrents)' \
	     --eval '(prove:run #P"tests/cl-torrents.lisp")' \
	     --eval '(quit)'

test-end2end:
	sbcl --load cl-torrents.asd \
	     --eval '(ql:quickload :cl-torrents)' \
	     --eval '(prove:run #P"tests/end2end.lisp")' \
	     --eval '(quit)'

install:
	# install dependencies, mostly for docker.
	apt update
	apt install -y git-core
	git clone https://github.com/Chream/mockingbird ~/quicklisp/local-projects/mockingbird/

ecl-docker:
	# v13 on Debian: too old.
	# https://stackoverflow.com/questions/46520876/building-an-executable-with-ecl-missing-dependency-or-can-not-find-make-build-i
	# v16 on Docker image. https://hub.docker.com/r/daewok/lisp-devel/
	# missing closer-mop ?
	# important compile time.
	ecl \
		-eval '(format t "--- ASDF version: ~a~&" (asdf:asdf-version))' \
		-eval '(ql:quickload :closer-mop)' \
		-eval '(load "cl-torrents.asd")' \
		-eval '(ql:quickload :cl-torrents)' \
		-eval '(asdf:make-build :cl-torrents :type :program :move-here #P"./")'
