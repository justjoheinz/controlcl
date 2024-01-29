
LISP=qlot exec ros run --

.PHONY: help all clean ex-colors ex-theme ex-ctrl qlot-update run-all

help: ## show help message
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"} /^[$$()% a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

all: 			## Compile all artifacts and execute tests
	$(LISP) \
		--disable-debugger \
		--eval '(ql:quickload :controlcl)' \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval '(ql:quickload :controlcl/tests)' \
		--eval '(uiop:quit (if (rove:run "controlcl/tests") 0 1))'

clean: ## Recursively delete fasl file
	find ./ -name "*.fasl" -delete

ex-colors: 		## Run the ex-colors example
	$(LISP) \
		--non-interactive \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval "(sdl2:make-this-thread-main #'controlcl/ex-colors:main)"


ex-theme: 		## Run the ex-theme example
	$(LISP) \
		--non-interactive \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval "(sdl2:make-this-thread-main #'controlcl/ex-theme:main)"

ex-ctrl: 		## Run the ex-ctrl example
	$(LISP) \
		--non-interactive \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval "(sdl2:make-this-thread-main #'controlcl/ex-ctrl:main)"

qlot-update: ## update qlot dependencies
	qlot update

run-all: qlot-update ## run all examples sequentially, escape with ESCy
	make ex-colors
	make ex-theme
	make ex-ctrl
