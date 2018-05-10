LISP?=sbcl

build:
	$(LISP) --load torrents.asd \
		--eval '(ql:quickload :torrents)' \
		--eval '(asdf:make :torrents)' \
		--eval '(quit)'

test:
	sbcl --load torrents.asd \
	     --eval '(ql:quickload :torrents)' \
	     --eval '(ql:quickload :torrents-test)' \
	     --eval '(prove:run #P"tests/test-torrents.lisp")' \
	     --eval '(prove:run #P"tests/test-1337.lisp")' \
	     --eval '(prove:run #P"tests/test-downloadsme.lisp")' \
	     --eval '(quit)'

test-end2end:
	sbcl --load torrents.asd \
	     --eval '(ql:quickload :torrents)' \
	     --eval '(ql:quickload :torrents-test)' \
	     --eval '(prove:run #P"tests/end2end.lisp")' \
	     --eval '(quit)'

install:
	# install dependencies, mostly for docker.
	git clone https://github.com/vindarel/replic/ ~/quicklisp/local-projects/
