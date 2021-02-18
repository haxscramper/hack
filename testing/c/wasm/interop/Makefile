autoreload:
	echo -e "Makefile\nmain.c\nbase.html" | entr -rc make run

compile:
	emcc main.c \
		-s EXPORTED_FUNCTIONS="['_testCall', '_printNumber', '_getReturn', '_main']" \
		-s EXTRA_EXPORTED_RUNTIME_METHODS="['cwrap','ccall']" \
		-s WASM=1 \
	 	-s NO_EXIT_RUNTIME=1 \
		--shell-file base.html \
		-o main.html

run: compile
	python3 -m http.server
