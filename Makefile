


all:
	ros run -- \
		--disable-debugger \
		--eval '(ql:quickload :controlcl)' \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval '(ql:quickload :controlcl/tests)' \
		--eval '(uiop:quit (if (rove:run "controlcl/tests") 0 1))'

ex-colors:
	ros run -- \
		--non-interactive \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval "(sdl2:make-this-thread-main #'controlcl/ex-colors:main)"


ex-theme:
	ros run -- \
		--non-interactive \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval "(sdl2:make-this-thread-main #'controlcl/ex-theme:main)"
