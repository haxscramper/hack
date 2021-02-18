autoreload:
	echo -e "hello.c\nshell_minimal.html" | entr -rc make run

compile:
	emcc hello.c --shell-file shell_minimal.html -o hello.html

run: compile
	python3 -m http.server
