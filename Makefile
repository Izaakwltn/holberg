LISP ?= sbcl

build:
	$(LISP) --load holberg-app-suite.asd \
	--eval '(ql:quickload :holberg-app-suite)' \
		--eval '(asdf:make :holberg-app-suite)' \
		--eval '(quit)'
