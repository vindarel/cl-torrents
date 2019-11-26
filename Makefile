LISP?=sbcl

all: test

build:
	$(LISP) --non-interactive \
		--load torrents.asd \
		--eval '(ql:quickload :torrents)' \
		--eval '(asdf:make :torrents)'

build-gui:
	$(LISP) --non-interactive \
		--load torrents.asd \
		--eval '(ql:quickload :torrents/tk)' \
		--eval '(asdf:make :torrents/tk)'

test:
	$(LISP) --non-interactive \
		--load torrents.asd \
		--load torrents-test.asd \
	     	--eval '(ql:quickload :torrents)' \
	     	--eval '(ql:quickload :torrents-test)' \
	     	--eval '(prove:run #P"tests/test-torrents.lisp")' \
	     	--eval '(prove:run #P"tests/test-1337.lisp")' \
	     	--eval '(prove:run #P"tests/test-downloadsme.lisp")'

test-end2end:
	$(LISP) --non-interactive \
	        --load torrents.asd \
		--load torrents-test.asd \
	      	--eval '(ql:quickload :torrents)' \
	     	--eval '(ql:quickload :torrents-test)' \
	     	--eval '(prove:run #P"tests/end2end.lisp")'

install:
	# install dependencies, mostly for docker.
	git clone https://github.com/vindarel/replic/ ~/quicklisp/local-projects/replic
	echo "------ cloning nodgui"
	git clone https://notabug.org/cage/nodgui ~/quicklisp/local-projects/nodgui
	echo "------ cloning cl-transmission"
	git clone https://github.com/libre-man/cl-transmission/ ~/quicklisp/local-projects/cl-transmission
