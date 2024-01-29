


help: ## show help message
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"} /^[$$()% a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

all: 			## Compile all artifacts
	ros run -- \
		--disable-debugger \
		--eval '(ql:quickload :controlcl)' \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval '(ql:quickload :controlcl/tests)' \
		--eval '(uiop:quit (if (rove:run "controlcl/tests") 0 1))'

ex-colors: 		## Run the ex-colors example
	ros run -- \
		--non-interactive \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval "(sdl2:make-this-thread-main #'controlcl/ex-colors:main)"


ex-theme: 		## Run the ex-theme example
	ros run -- \
		--non-interactive \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval "(sdl2:make-this-thread-main #'controlcl/ex-theme:main)"

ex-ctrl: 		## Run the ex-ctrl example
	ros run -- \
		--non-interactive \
		--eval '(ql:quickload :controlcl/examples)' \
		--eval "(sdl2:make-this-thread-main #'controlcl/ex-ctrl:main)"
